{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
-- just for the type equality constraint
{-# LANGUAGE GADTs                 #-}

module Language.PlutusCore.Quote
    ( runQuoteT
    , runQuote
    , freshUnique
    , freshName
    , freshTyName
    , freshenName
    , freshenTyName
    , markNonFresh
    , markNonFreshBelow
    , markNonFreshTerm
    , markNonFreshType
    , markNonFreshProgram
    , QuoteT
    , Quote
    , MonadQuote
    , FreshState
    , liftQuote
    ) where

import           Control.Lens              (coerced)
import           Control.Monad.Except
import           Control.Monad.Morph       as MM
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Maybe

import           Data.Functor.Foldable
import           Data.Functor.Identity
import           Data.Maybe                (fromMaybe)
import qualified Data.Set                  as Set
import qualified Data.Text                 as Text
import           Hedgehog                  (GenT, PropertyT)

import           Language.PlutusCore.Name
import           Language.PlutusCore.Type
import           PlutusPrelude

-- | The state contains the "next" 'Unique' that should be used for a name
type FreshState = Unique

emptyFreshState :: FreshState
emptyFreshState = Unique 0

-- | The "quotation" monad transformer. Within this monad you can do safe construction of PLC terms using quasiquotation,
-- fresh-name generation, and parsing.
newtype QuoteT m a = QuoteT { unQuoteT :: StateT FreshState m a }
    deriving (Functor, Applicative, Monad, MonadTrans, MM.MFunctor, MonadError e, MonadReader r, MonadIO)

-- Need to write this by hand, deriving wants to derive the one for DefState
instance MonadState s m => MonadState s (QuoteT m) where
    get = lift get
    put = lift . put
    state = lift . state

-- | A monad that allows lifting of quoted expressions.
class Monad m => MonadQuote m where
    liftQuote :: Quote a -> m a
    -- This means we don't have to implement it when we're writing an instance for a MonadTrans monad. We can't just
    -- add an instance declaration for that, because it overlaps with the base instance.
    default liftQuote :: (MonadQuote n, MonadTrans t, t n ~ m) => Quote a -> m a
    liftQuote = lift . liftQuote

instance (Monad m) => MonadQuote (QuoteT m) where
    liftQuote = MM.hoist (pure . runIdentity)

instance MonadQuote m => MonadQuote (StateT s m)
instance MonadQuote m => MonadQuote (MaybeT m)
instance MonadQuote m => MonadQuote (ExceptT e m)
instance MonadQuote m => MonadQuote (ReaderT r m)
instance MonadQuote m => MonadQuote (GenT m)
instance MonadQuote m => MonadQuote (PropertyT m)

-- | Run a quote from an empty identifier state. Note that the resulting term cannot necessarily
-- be safely combined with other terms - that should happen inside 'QuoteT'.
runQuoteT ::  Monad m => QuoteT m a -> m a
runQuoteT q = evalStateT (unQuoteT q) emptyFreshState

-- | A non-transformer version of 'QuoteT'.
type Quote = QuoteT Identity

-- | See 'runQuoteT'.
runQuote :: Quote a -> a
runQuote = runIdentity . runQuoteT

-- | Marks all the 'Unique's in a type as used, so they will not be generated in future. Useful if you
-- have a type which was not generated in 'Quote'.
markNonFreshType
    :: (HasUnique (tyname ann) TypeUnique, MonadQuote m)
    => Type tyname ann
    -> m ()
markNonFreshType = markNonFreshMax . collectTypeUniques

-- | Marks all the 'Unique's in a term as used, so they will not be generated in future. Useful if you
-- have a term which was not generated in 'Quote'.
markNonFreshTerm
    :: (HasUnique (tyname ann) TypeUnique, HasUnique (name ann) TermUnique, MonadQuote m)
    => Term tyname name ann
    -> m ()
markNonFreshTerm = markNonFreshMax . collectTermUniques

-- | Marks all the 'Unique's in a program as used, so they will not be generated in future. Useful if you
-- have a program which was not generated in 'Quote'.
markNonFreshProgram
    :: (HasUnique (tyname ann) TypeUnique, HasUnique (name ann) TermUnique, MonadQuote m)
    => Program tyname name ann
    -> m ()
markNonFreshProgram (Program _ _ body) = markNonFreshTerm body

-- | Mark the maximal 'Unique' from a set of 'Unique's (and implicitly all 'Unique's less than it)
-- as used, so they will not be generated in future.
markNonFreshMax :: MonadQuote m => Set.Set Unique -> m ()
markNonFreshMax = markNonFresh . fromMaybe (Unique 0) . Set.lookupMax

-- | Mark a given 'Unique' (and implicitly all 'Unique's less than it) as used, so they will not be generated in future.
markNonFresh :: MonadQuote m => Unique -> m ()
markNonFresh nonFresh = liftQuote $ do
    nextU <- QuoteT get
    QuoteT $ put $ Unique (max (unUnique nonFresh + 1) (unUnique nextU))

-- | Mark a all 'Unique's less than the given 'Unique' as used, so they will not be generated in future.
markNonFreshBelow :: MonadQuote m => Unique -> m ()
markNonFreshBelow nonFresh = liftQuote $ do
    nextU <- QuoteT get
    QuoteT $ put $ Unique (max (unUnique nonFresh) (unUnique nextU))

collectTypeUniques :: (HasUnique (tyname ann) TypeUnique) => Type tyname ann -> Set.Set Unique
collectTypeUniques = cata f where
    f = \case
        TyVarF _ tn        -> Set.singleton (tn ^. unique . coerced)
        TyForallF _ tn _ t -> Set.insert (tn ^. unique . coerced) t
        TyLamF _ tn _ t    -> Set.insert (tn ^. unique . coerced) t
        TyAppF _ t1 t2     -> t1 <> t2
        TyFunF _ t1 t2     -> t1 <> t2
        TyIFixF _ pat arg  -> pat <> arg
        _                  -> mempty

collectTermUniques
    :: (HasUnique (tyname ann) TypeUnique, HasUnique (name ann) TermUnique)
    => Term tyname name ann -> Set.Set Unique
collectTermUniques = cata f where
    f = \case
        VarF _ n           -> Set.singleton (n ^. unique . coerced)
        LamAbsF _ n ty t   -> Set.insert (n ^. unique . coerced) (collectTypeUniques ty <> t)
        TyAbsF _ tn _ t    -> Set.insert (tn ^. unique . coerced) t
        TyInstF _ t ty     -> t <> collectTypeUniques ty
        ApplyF _ t1 t2     -> t1 <> t2
        IWrapF _ pat arg t -> collectTypeUniques pat <> collectTypeUniques arg <> t
        UnwrapF _ t        -> t
        ErrorF _ ty        -> collectTypeUniques ty
        _                  -> mempty

-- | Get a fresh 'Unique'.
freshUnique :: MonadQuote m => m Unique
freshUnique = liftQuote $ do
    nextU <- QuoteT get
    QuoteT $ put $ Unique (unUnique nextU + 1)
    pure nextU

-- | Get a fresh 'Name', given the annotation and the 'Text.Text' name.
freshName :: Monad m => ann -> Text.Text -> QuoteT m (Name ann)
freshName ann str = Name ann str <$> freshUnique

-- | Make a copy of the given 'Name' that is distinct from the old one.
freshenName :: Monad m => Name ann -> QuoteT m (Name ann)
freshenName (Name ann str _) = Name ann str <$> freshUnique

-- | Get a fresh 'TyName', given the annotation and the 'Text.Text' name.
freshTyName :: Monad m => ann -> Text.Text -> QuoteT m (TyName ann)
freshTyName = fmap TyName .* freshName

-- | Make a copy of the given 'TyName' that is distinct from the old one.
freshenTyName :: Monad m => TyName ann -> QuoteT m (TyName ann)
freshenTyName (TyName name) = TyName <$> freshenName name
