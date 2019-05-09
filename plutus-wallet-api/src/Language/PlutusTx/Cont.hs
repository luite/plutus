module Language.PlutusTx.Cont(
      MonadWalletCont
    , runMonadWallet
    , await
    ) where

import           Control.Monad.Cont

import           Wallet.API

type MonadWalletCont m = ContT () m

-- | Interpret a 'MonadWalletCont' action in the underlying 'MonadWallet'
runMonadWallet :: MonadWallet m => MonadWalletCont m () -> m ()
runMonadWallet m = runContT m pure

-- | Wait for a blockchain event.
await :: WalletAPI m => EventTrigger -> MonadWalletCont m ()
await trg = ContT $ \f -> registerOnce trg (EventHandler $ const $ f ())

-- -- | Watch an address for new outputs. When the set of outputs at the
-- --   address changes, return the list of all data scripts at the address.
-- listen :: (Unlift a, WalletAPI m) => Address -> MonadWalletCont m [a]
-- TODO: Implement 'listen' when we can derive unlift. Then use it to implement 
-- a state machine that reacts to state changes from the chain.

-- TODO 2: For this to work properly we also need a better Applicative instance.
-- Something like `(,) <$> await trg1 <*> await trg2` should be treated like
-- `await (trg1 `or` trg2)`, so it shouldn't matter which of the two happens 
--  first.
