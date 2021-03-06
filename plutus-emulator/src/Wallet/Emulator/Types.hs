{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Wallet.Emulator.Types(
    -- * Wallets
    Wallet(..),
    walletPubKey,
    walletPrivKey,
    signWithWallet,
    addSignature,
    TxPool,
    -- * Emulator
    Assertion(OwnFundsEqual, IsValidated),
    assert,
    assertIsValidated,
    AssertionError(..),
    AsAssertionError(..),
    Event(..),
    Notification(..),
    EmulatorEvent(..),
    EmulatorAction(..),
    -- ** Wallet state
    WalletState(..),
    emptyWalletState,
    ownPrivateKey,
    ownAddress,
    ownFunds,
    addressMap,
    walletSlot,
    -- ** Traces
    Trace,
    runTraceChain,
    runTraceTxPool,
    evalTraceTxPool,
    execTraceTxPool,
    walletAction,
    runWalletAction,
    runSuccessfulWalletAction,
    runFailingWalletAction,
    walletRecvNotifications,
    walletNotifyBlock,
    walletsNotifyBlock,
    processPending,
    addBlocks,
    addBlocksAndNotify,
    assertion,
    assertOwnFundsEq,
    ownFundsEqual,
    runEmulator,
    runTraceChainDefault,
    runTraceChainDefaultWallet,
    -- * Emulator internals
    MockWallet(..),
    handleNotifications,
    EmulatorState(..),
    emptyEmulatorState,
    emulatorState,
    emulatorStatePool,
    emulatorStateInitialDist,
    NC.chainNewestFirst,
    chainOldestFirst,
    NC.txPool,
    walletStates,
    walletIndex,
    NC.index,
    chainState,
    MonadEmulator,
    liftMockWallet,
    evalEmulated,
    processEmulated,
    runWalletActionAndProcessPending,
    fundsDistribution,
    emLog,
    selectCoin
    ) where

import           Control.Lens               hiding (index)
import           Control.Monad.Error.Lens
import           Control.Monad.Except
import           Control.Monad.Operational  as Op hiding (view)
import           Control.Monad.State
import           Control.Monad.Writer
import           Control.Newtype.Generics   (Newtype)
import           Data.Aeson                 (FromJSON, ToJSON, ToJSONKey)
import           Data.Bifunctor             (Bifunctor (..))
import qualified Data.ByteString.Lazy       as BSL
import           Data.Foldable              (traverse_)
import           Data.Hashable              (Hashable)
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HashMap
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Maybe
import qualified Data.Set                   as Set
import           Data.String                (fromString)
import qualified Data.Text                  as T
import           Data.Text.Prettyprint.Doc  hiding (annotate)
import           GHC.Generics               (Generic)
import           IOTS                       (IotsType (iotsDefinition))
import qualified Ledger.Crypto              as Crypto
import           Prelude                    as P
import           Schema                     (ToSchema)
import           Servant.API                (FromHttpApiData (..), ToHttpApiData (..))

import qualified Language.PlutusTx.Prelude  as PlutusTx

import           Ledger                     (Address, Block, Blockchain, PrivateKey (..), PubKey (..), Slot, Tx (..),
                                             TxIn (..), TxOut (..), TxOutRef, TxOutTx (..), Value, addSignature,
                                             pubKeyAddress, pubKeyTxIn, pubKeyTxOut, toPublicKey, txId, txOutAddress)
import qualified Ledger.Ada                 as Ada
import qualified Ledger.AddressMap          as AM
import qualified Ledger.Index               as Index
import qualified Ledger.Value               as Value
import           Wallet.API                 (EventHandler (..), EventTrigger, WalletAPI (..), WalletAPIError (..),
                                             WalletDiagnostics (..), WalletLog (..), addresses, annTruthValue, getAnnot)
import qualified Wallet.API                 as WAPI
import qualified Wallet.Emulator.NodeClient as NC

-- | A wallet in the emulator model.
newtype Wallet = Wallet { getWallet :: Integer }
    deriving (Show, Eq, Ord, Generic)
    deriving newtype (ToHttpApiData, FromHttpApiData, Hashable)
    deriving anyclass (Newtype, ToJSON, FromJSON, ToJSONKey, ToSchema, IotsType)

instance Pretty Wallet where
    pretty (Wallet i) = "W" <> pretty i

-- | Get a wallet's public key.
walletPubKey :: Wallet -> PubKey
walletPubKey = toPublicKey . walletPrivKey

-- | Get a wallet's private key by looking it up in the list of
--   private keys in 'Ledger.Crypto.knownPrivateKeys'
walletPrivKey :: Wallet -> PrivateKey
walletPrivKey (Wallet i) = cycle Crypto.knownPrivateKeys !! fromIntegral i

-- | Sign a 'Tx' using the wallet's privat key.
signWithWallet :: Wallet -> Tx -> Tx
signWithWallet wlt = addSignature (walletPrivKey wlt)

-- | A pool of transactions which have yet to be validated.
type TxPool = [Tx]

-- | A notification sent to a wallet about a change in the ledger.
data Notification = BlockValidated Block -- ^ A new block has been validated.
                  | CurrentSlot Slot -- ^ The current slot has changed.
                  deriving (Show, Eq)

-- manually records the list of transactions to be submitted
-- | A mock wallet environment to allow pure testing of the wallet API. This type simply records the list of transactions
-- which are requested to be submitted.
newtype MockWallet a = MockWallet { runMockWallet :: (ExceptT WalletAPIError (StateT WalletState (Writer (WalletLog, [Tx])))) a }
    deriving newtype (Functor, Applicative, Monad, MonadState WalletState, MonadError WalletAPIError, MonadWriter (WalletLog, [Tx]))

instance WalletDiagnostics MockWallet where
    logMsg t = tell (WalletLog [t], [])

-- | JavaScript has no effect system, so we basically have to throw
-- away the monadic context and trust that things will magically work
-- out.
instance IotsType a => IotsType (MockWallet a) where
  iotsDefinition = iotsDefinition @a

tellTx :: [Tx] -> MockWallet ()
tellTx tx = MockWallet $ tell (mempty, tx)

-- Wallet code

-- EventTrigger is not Ord so we use a HashMap in here
-- | The state used by the mock wallet environment.
data WalletState = WalletState {
    _ownPrivateKey :: PrivateKey,
    -- ^ User's 'PrivateKey'.
    _walletSlot    :: Slot,
    -- ^ Current slot as far as the wallet is concerned.
    _addressMap    :: AM.AddressMap,
    -- ^ Addresses that we watch.
    _triggers      :: HashMap EventTrigger (EventHandler MockWallet)
    -- ^ Triggers registered by the user.
    }

instance Show WalletState where
    showsPrec p (WalletState kp bh wa tr) = showParen (p > 10)
        (showString "WalletState"
            . showChar ' ' . showsPrec 10 kp
            . showChar ' ' . showsPrec 10 bh
            . showChar ' ' . showsPrec 10 wa
            . showChar ' ' . showsPrec 10 (HashMap.map (const ("<..>" :: String)) tr))

makeLenses ''WalletState

-- | Get the user's own public-key address.
ownAddress :: WalletState -> Address
ownAddress = pubKeyAddress . toPublicKey . view ownPrivateKey

-- | Get the funds available at the user's own public-key address.
ownFunds :: Lens' WalletState (Map TxOutRef TxOutTx)
ownFunds = lens g s where
    g ws = fromMaybe Map.empty $ ws ^. addressMap . at (ownAddress ws)
    s ws utxo = ws & addressMap . at (ownAddress ws) ?~ utxo


-- | An empty wallet state with the public/private key pair for a wallet, and the public-key address
-- for that wallet as the sole watched address.
emptyWalletState :: Wallet -> WalletState
emptyWalletState w = WalletState pk 0 oa mempty where
    oa = AM.addAddress ownAddr mempty
    pk = walletPrivKey w
    ownAddr = pubKeyAddress (toPublicKey pk)

-- | Events produced by the blockchain emulator.
data EmulatorEvent =
    ChainEvent NC.ChainEvent
    -- ^ A blockchain event
    | WalletError Wallet WalletAPIError
    -- ^ A 'WalletAPI' action produced an error.
    | WalletInfo Wallet T.Text
    -- ^ Debug information produced by a wallet.
    deriving (Eq, Show, Generic)

instance FromJSON EmulatorEvent
instance ToJSON EmulatorEvent

instance Pretty EmulatorEvent where
    pretty = \case
        ChainEvent (NC.TxnSubmit t) -> "TxnSubmit" <+> pretty t
        ChainEvent (NC.TxnValidate t) -> "TxnValidate" <+> pretty t
        ChainEvent (NC.TxnValidationFail t e) -> "TxnValidationFail" <+> pretty t <> colon <+> pretty e
        ChainEvent (NC.SlotAdd sl) -> "SlotAdd" <+> pretty sl
        WalletError w e -> "WalletError" <+> pretty w <> colon <+> pretty e
        WalletInfo w t -> "WalletInfo" <+> pretty w <> colon <+> pretty t

-- | Delete all 'EventHandler' values that are registered for an
--   'EventTrigger'.
deleteHandlers :: MonadState WalletState m => EventTrigger -> m ()
deleteHandlers t = modify (over triggers (set (at t) Nothing))

-- | Process a list of 'Notification's in the mock wallet environment.
handleNotifications :: [Notification] -> MockWallet ()
handleNotifications = mapM_ (updateState >=> runTriggers)  where
    updateState = \case
            CurrentSlot h -> modify (walletSlot .~ h)
            BlockValidated blck -> mapM_ (modify . update) blck >> modify (walletSlot %~ succ)

    runTriggers _ = do
        h <- gets (view walletSlot)
        adrs <- gets (view addressMap)
        trg <- gets (view triggers)

        let values = AM.values adrs
            annotate = annTruthValue h values
            trueConditions = filter (getAnnot . fst) $ first annotate <$> HashMap.toList trg

        -- We need to do 2 passes over the list of triggers that fired.
        --
        -- First pass to delete the old triggers
        -- Second pass to run the actions
        --
        -- Deletion must happen first so that we don't accidentally delete
        -- triggers that are registered by event handlers.
        traverse_ (deleteHandlers . WAPI.unAnnot . fst) trueConditions
        traverse_ (uncurry (flip runEventHandler)) trueConditions

    -- Remove spent outputs and add unspent ones, for the addresses that we care about
    update t = over addressMap (\am -> AM.fromTxOutputs t <> AM.updateAddresses t am )

-- Make a transaction output from a positive value.
mkChangeOutput :: PubKey -> Value -> Maybe TxOut
mkChangeOutput pubK v =
    if Value.isZero v then Nothing else Just (pubKeyTxOut v pubK)

instance WalletAPI MockWallet where

    submitTxn txn =
        let adrs = txOutAddress <$> txOutputs txn in
        modifying addressMap (AM.addAddresses adrs) >>
        tellTx [txn]

    ownPubKey = toPublicKey <$> use ownPrivateKey

    sign bs = do
        privK <- use ownPrivateKey
        pure (Crypto.sign (BSL.toStrict bs) privK)

    updatePaymentWithChange vl (oldIns, changeOut) = do
        ws <- get
        let -- These inputs have been already used, we won't touch them
            usedFnds = Set.map txInRef oldIns
            -- Optional, left over change. Replace a `Nothing` with a Value of 0.
            oldChange = maybe (Ada.lovelaceValueOf 0) txOutValue changeOut
            -- Available funds.
            fnds   = Map.withoutKeys (ws ^. ownFunds) usedFnds
            privK  = ws ^. ownPrivateKey
            pubK   = toPublicKey privK
        if vl `Value.leq` oldChange
        then
          -- If the requested value is covered by the change we only need to update
          -- the remaining change.
          pure (oldIns, mkChangeOutput pubK $ oldChange PlutusTx.- vl)
        else do
          -- If the requested value is not covered by the change, then we need to
          -- select new inputs, after deducting the oldChange from the value.
          (spend, change) <- selectCoin (second (txOutValue . txOutTxOut) <$> Map.toList fnds)
                                        (vl PlutusTx.- oldChange)
          let ins = Set.fromList (pubKeyTxIn pubK . fst <$> spend)
          pure (Set.union oldIns ins, mkChangeOutput pubK change)

    registerOnce tr action =
        modify (over triggers (HashMap.insertWith (<>) tr action))
        >> modify (over addressMap (AM.addAddresses (addresses tr)))

    watchedAddresses = use addressMap

    startWatching = modifying addressMap . AM.addAddress

    slot = use walletSlot

-- | Given a set of @a@s with coin values, and a target value, select a number
-- of @a@ such that their total value is greater than or equal to the target.
selectCoin :: (MonadError WalletAPIError m)
    => [(a, Value)]
    -> Value
    -> m ([(a, Value)], Value)
selectCoin fnds vl =
        let
            total = foldMap snd fnds
            fundsWithTotal = P.zip fnds (drop 1 $ P.scanl (<>) mempty $ fmap snd fnds)
            err   = throwError
                    $ InsufficientFunds
                    $ T.unwords
                        [ "Total:", T.pack $ show total
                        , "expected:", T.pack $ show vl]
        -- Values are in a partial order: what we want to check is that the
        -- total available funds are bigger than (or equal to) the required value.
        -- It is *not* correct to replace this condition with 'total `Value.lt` vl' -
        -- consider what happens if the amounts are incomparable.
        in  if not (total `Value.geq` vl)
            then err
            else
                let
                    fundsToSpend   = takeUntil (\(_, runningTotal) -> vl `Value.leq` runningTotal) fundsWithTotal
                    totalSpent     = case reverse fundsToSpend of
                                        []            -> PlutusTx.zero
                                        (_, total'):_ -> total'
                    change         = totalSpent PlutusTx.- vl
                in pure (fst <$> fundsToSpend, change)

-- | Take elements from a list until the predicate is satisfied.
-- 'takeUntil' @p@ includes the first element for wich @p@ is true
-- (unlike @takeWhile (not . p)@).
takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ []       = []
takeUntil p (x:xs)
    | p x            = [x]
    | otherwise      = x : takeUntil p xs

-- Emulator code

-- | Assertions which will be checked during execution of the emulator.
data Assertion
  = IsValidated Tx -- ^ Assert that the given transaction is validated.
  | OwnFundsEqual Wallet Value -- ^ Assert that the funds belonging to a wallet's public-key address are equal to a value.

-- | An error emitted when an 'Assertion' fails.
newtype AssertionError = GenericAssertion T.Text
    deriving (Show, Eq)
makeClassyPrisms ''AssertionError

-- | This lets people use 'T.Text' as their error type.
instance AsAssertionError T.Text where
    _AssertionError = prism' (T.pack . show) (const Nothing)

-- | The type of events in the emulator. @n@ is the type (usually a monad implementing 'WalletAPI') in
-- which wallet actions take place.
data Event n a where
    -- | An direct action performed by a wallet. Usually represents a "user action", as it is
    -- triggered externally.
    WalletAction :: Wallet -> n a -> Event n (Either WalletAPIError a, [Tx])
    -- | A wallet receiving some notifications, and reacting to them.
    WalletRecvNotification :: Wallet -> [Notification] -> Event n [Tx]
    -- | The blockchain processing pending transactions, producing a new block
    --   from the valid ones and discarding the invalid ones.
    BlockchainProcessPending :: Event n Block
    -- | An assertion in the event stream, which can inspect the current state.
    Assertion :: Assertion -> Event n ()
    -- | Failure.
    Failure :: T.Text -> Event n a

-- Program is like Free, except it makes the Functor for us so we can have a nice GADT
-- | A series of 'Event's.
type Trace m = Op.Program (Event m)

-- | The state of the emulator itself.
data EmulatorState = EmulatorState {
    _walletIndex  :: AM.AddressMap, -- ^ A richer index used by wallets.
    _chainState   :: NC.ChainState,
    _walletStates :: Map Wallet WalletState, -- ^ The state of each wallet.
    _emulatorLog  :: [EmulatorEvent] -- ^ The emulator events, with the newest first.
    } deriving (Show)

makeLenses ''EmulatorState

-- | Get a map with the total value of each wallet's "own funds".
fundsDistribution :: EmulatorState -> Map Wallet Value
fundsDistribution = Map.map (foldMap (txOutValue . txOutTxOut) . view ownFunds) . view walletStates

-- | Get the emulator log.
emLog :: EmulatorState -> [EmulatorEvent]
emLog = view emulatorLog

-- | Get the blockchain as a list of blocks, starting with the oldest (genesis)
--   block.
chainOldestFirst :: Lens' EmulatorState Blockchain
chainOldestFirst = chainState . NC.chainNewestFirst . reversed

type MonadEmulator e m = (MonadError e m, AsAssertionError e, MonadState EmulatorState m)

emptyEmulatorState :: EmulatorState
emptyEmulatorState = EmulatorState {
    _chainState = NC.emptyChainState,
    _walletStates = Map.empty,
    _walletIndex = mempty,
    _emulatorLog = []
    }

-- | Issue an 'Assertion'.
assert :: (MonadEmulator e m) => Assertion -> m ()
assert (IsValidated txn)            = isValidated txn
assert (OwnFundsEqual wallet value) = ownFundsEqual wallet value

-- | Issue an assertion that the funds for a given wallet have the given value.
ownFundsEqual :: (MonadEmulator e m) => Wallet -> Value -> m ()
ownFundsEqual wallet value = do
    es <- get
    ws <- case Map.lookup wallet $ _walletStates es of
        Nothing -> throwing _AssertionError $ GenericAssertion "Wallet not found"
        Just ws -> pure ws
    let total = foldMap (txOutValue . txOutTxOut) $ ws ^. ownFunds
    if value == total
    then pure ()
    else throwing _AssertionError . GenericAssertion $ T.unwords ["Funds in wallet", tshow wallet, "were", tshow total, ". Expected:", tshow value]
    where
    tshow :: Show a => a -> T.Text
    tshow = T.pack . show

-- | Issue an assertion that the given transaction has been validated.
isValidated :: (MonadEmulator e m) => Tx -> m ()
isValidated txn = do
    emState <- get
    if notElem txn (join $ emState ^. chainState . NC.chainNewestFirst)
        then throwing _AssertionError $ GenericAssertion $ "Txn not validated: " <> T.pack (show txn)
        else pure ()

-- | Initialise the emulator state with a blockchain.
emulatorState :: Blockchain -> EmulatorState
emulatorState bc = emptyEmulatorState
    & walletIndex .~ AM.fromChain bc
    & chainState . NC.chainNewestFirst .~ bc
    & chainState . NC.index .~ Index.initialise bc

-- | Initialise the emulator state with a pool of pending transactions.
emulatorStatePool :: TxPool -> EmulatorState
emulatorStatePool tp = emptyEmulatorState
    & chainState . NC.txPool .~ tp

-- | Initialise the emulator state with a single pending transaction that
--   creates the initial distribution of funds to public key addresses.
emulatorStateInitialDist :: Map PubKey Value -> EmulatorState
emulatorStateInitialDist mp = emulatorStatePool [tx] where
    tx = Tx
            { txInputs = Set.empty
            , txOutputs = uncurry (flip pubKeyTxOut) <$> Map.toList mp
            , txForge = foldMap snd $ Map.toList mp
            , txFee = PlutusTx.zero
            , txValidRange = WAPI.defaultSlotRange
            , txSignatures = Map.empty
            , txData = Map.empty
            }

-- | Lift an action that runs in 'MockWallet' into one that runs in an @MonadState EmulatorState@ monad by
-- running it for a particular 'Wallet'. This produces a list of transactions to be submitted, and either
-- a value or an error.
liftMockWallet :: (MonadState EmulatorState m) => Wallet -> MockWallet a -> m ([Tx], Either WalletAPIError a)
liftMockWallet wallet act = do
    emState <- get
    let walletState = fromMaybe (emptyWalletState wallet) $ Map.lookup wallet $ _walletStates emState
        ((out, newState), (msgs, txns)) = runWriter $ runStateT (runExceptT (runMockWallet act)) walletState
        events = (ChainEvent . NC.TxnSubmit . txId <$> txns) ++ (WalletInfo wallet <$> getWalletLog msgs)
    liftNodeClient $ mapM_ NC.publishTx txns
    modify $ set walletStates (Map.insert wallet newState $ _walletStates emState)
           . set emulatorLog  (events ++ _emulatorLog emState)
    pure (txns, out)

liftNodeClient :: MonadState EmulatorState m => State NC.ChainState a -> m a
liftNodeClient ncAction = state $ \st ->
    let (v, newNcState) = runState ncAction (_chainState st)
    in  (v, st & chainState .~ newNcState)

-- | Evaluate an 'Event' in a 'MonadEmulator' monad.
evalEmulated :: (MonadEmulator e m) => Event MockWallet a -> m a
evalEmulated = \case
    WalletAction wallet action -> do
        (txns, result) <- liftMockWallet wallet action
        case result of
            Right a -> pure (Right a, txns)
            Left err -> do
                _ <- modifying emulatorLog (WalletError wallet err :)
                pure (Left err, txns)
    WalletRecvNotification wallet trigger -> fst <$> liftMockWallet wallet (handleNotifications trigger)
    BlockchainProcessPending -> do
        initialState <- get
        validatedBlock <- liftNodeClient NC.processBlock
        finalState <- get
        let
            (NC.ValidatedBlock block events _) = validatedBlock
            walletIndex' = foldr AM.updateAllAddresses (_walletIndex initialState) block
            idx' = view (chainState . NC.index) finalState
        unless (Index.getIndex idx' == (fmap txOutTxOut $ AM.outRefMap walletIndex')) $
            throwing _AssertionError $ GenericAssertion $ "Indices don't match\n" <> (fromString $ show idx') <> "\n" <> (fromString $ show walletIndex')
        put finalState {
            _emulatorLog   = map ChainEvent events ++ _emulatorLog finalState,
            _walletIndex   = walletIndex'
        }
        pure block
    Assertion a -> assert a
    Failure message -> throwing _AssertionError $ GenericAssertion message

processEmulated :: (MonadEmulator e m) => Trace MockWallet a -> m a
processEmulated = interpretWithMonad evalEmulated

-- | A synonym for 'runSuccessfulWalletAction'. Runs a wallet action and asserts that it was a success.
walletAction :: Wallet -> m a -> Trace m (a, [Tx])
walletAction = runSuccessfulWalletAction

-- | Peform a wallet action as the given 'Wallet'.
runWalletAction :: Wallet -> m a -> Trace m (Either WalletAPIError a, [Tx])
runWalletAction w = Op.singleton . WalletAction w

-- | Peform a wallet action as the given 'Wallet', asserting that the result was a success.
runSuccessfulWalletAction :: Wallet -> m a -> Trace m (a, [Tx])
runSuccessfulWalletAction w act = do
    (maybeOut, txs) <- runWalletAction w act
    case maybeOut of
        Left e  -> failure $ T.pack $ show e
        Right a -> pure (a, txs)

-- | Peform a wallet action as the given 'Wallet', asserting that the result was a failure.
runFailingWalletAction :: Wallet -> m a -> Trace m [Tx]
runFailingWalletAction w act = do
    (maybeOut, txs) <- runWalletAction w act
    case maybeOut of
        Left _  -> pure txs
        Right _ -> failure "Wallet action succeeded unexpectedly"

-- | Notify the given 'Wallet' of some blockchain events.
walletRecvNotifications :: Wallet -> [Notification] -> Trace m [Tx]
walletRecvNotifications w = Op.singleton . WalletRecvNotification w

-- | Notify the given 'Wallet' that a block has been validated.
walletNotifyBlock :: Wallet -> Block -> Trace m [Tx]
walletNotifyBlock w = walletRecvNotifications w . pure . BlockValidated

-- | Notify a list of 'Wallet's that a block has been validated.
walletsNotifyBlock :: [Wallet] -> Block -> Trace m [Tx]
walletsNotifyBlock wls b = foldM (\ts w -> (ts ++) <$> walletNotifyBlock w b) [] wls

-- | Validate all pending transactions.
processPending :: Trace m Block
processPending = Op.singleton BlockchainProcessPending

-- | Add a number of empty blocks to the blockchain, by performing
--   'processPending' @n@ times.
addBlocks :: Integer -> Trace m [Block]
addBlocks i = traverse (const processPending) [1..i]

-- | Add a number of blocks, notifying all the given 'Wallet's after each block.
addBlocksAndNotify :: [Wallet] -> Integer -> Trace m ()
addBlocksAndNotify wallets i =
    traverse_ (\_ -> processPending >>= walletsNotifyBlock wallets) [1..i]

-- | Issue an 'Assertion'.
assertion :: Assertion -> Trace m ()
assertion = Op.singleton . Assertion

-- | Issue an assertion that the funds for a given wallet have the given value.
assertOwnFundsEq :: Wallet -> Value -> Trace m ()
assertOwnFundsEq wallet = assertion . OwnFundsEqual wallet

-- | Issue an assertion that the given transaction has been validated.
assertIsValidated :: Tx -> Trace m ()
assertIsValidated = assertion . IsValidated

-- | Fail.
failure :: T.Text -> Trace m a
failure = Op.singleton . Failure

newtype EmulatorAction e a = EmulatorAction { unEmulatorAction :: ExceptT e (State EmulatorState) a }
    deriving newtype (Functor, Applicative, Monad, MonadState EmulatorState, MonadError e)

-- | Run a 'MonadEmulator' action on an 'EmulatorState', returning the final
--   state and either the result or an 'AssertionError'.
runEmulator :: forall e a . EmulatorState -> EmulatorAction e a -> (Either e a, EmulatorState)
runEmulator e a = runState (runExceptT $ unEmulatorAction a) e

-- | Run an 'Trace' on a blockchain.
runTraceChain :: Blockchain -> Trace MockWallet a -> (Either AssertionError a, EmulatorState)
runTraceChain ch t = runState (runExceptT $ processEmulated t) emState where
    emState = emulatorState ch

-- | Run a 'Trace' on an empty blockchain with a pool of pending transactions.
runTraceTxPool :: TxPool -> Trace MockWallet a -> (Either AssertionError a, EmulatorState)
runTraceTxPool tp t = runState (runExceptT $ processEmulated t) emState where
    emState = emulatorStatePool tp

-- | Evaluate a 'Trace' on an empty blockchain with a pool of pending
--   transactions and return the final value, discarding the final
--   'EmulatorState'.
evalTraceTxPool :: TxPool -> Trace MockWallet a -> Either AssertionError a
evalTraceTxPool pl = fst . runTraceTxPool pl

-- | Evaluate a 'Trace' on an empty blockchain with a pool of pending
--   transactions and return the final 'EmulatorState', discarding the final
--   value.
execTraceTxPool :: TxPool -> Trace MockWallet a -> EmulatorState
execTraceTxPool pl = snd . runTraceTxPool pl

-- | Run an action as a wallet, subsequently process any pending transactions
--   and notify wallets. Returns the new block
runWalletActionAndProcessPending :: [Wallet] -> Wallet -> m a -> Trace m ([Tx], Either WalletAPIError a)
runWalletActionAndProcessPending wallets wallet action = do
    (result, _) <- runWalletAction wallet action
    block <- processPending
    _ <- walletsNotifyBlock wallets block
    pure (block, result)

allWallets :: [Wallet]
allWallets = Wallet <$> [1..10]

-- | Run an 'EmulatorAction' on a blockchain using a default initial
--   distribution of 100 Ada for each of the wallets 1-10.
runTraceChainDefault :: AsAssertionError e => EmulatorAction e a -> (Either e a, EmulatorState)
runTraceChainDefault action =
    let
        dist = [(x, 100) | x <- allWallets]
        s = emulatorStateInitialDist (Map.fromList (first walletPubKey . second Ada.toValue <$> dist))

        -- make sure the wallets know about the initial transaction
        notifyInitial = void (addBlocksAndNotify (fst <$> dist) 1)
    in runEmulator s (processEmulated notifyInitial >> action)

-- | Run an 'WalletAction' in the context of wallet 1, on a default blockchain
--   with an initial distribution of 100 Ada, and then process all transactions
--   produced by the 'WalletAction'. Returns the result of the action,
--   transactions submitted by the action, and the final emulator state.
runTraceChainDefaultWallet :: MockWallet a -> (Either AssertionError (Either WalletAPIError a, [Tx]), EmulatorState)
runTraceChainDefaultWallet a = runTraceChainDefault (processEmulated a') where
    a' = do
        r <- runWalletAction (Wallet 1) a
        _ <- processPending >>= walletsNotifyBlock allWallets
        pure r
