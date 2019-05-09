{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
-- | A "pay-to-pubkey" transaction output implemented as a Plutus
--   contract. This is useful if you need something that behaves like
--   a pay-to-pubkey output, but is not (easily) identified by wallets
--   as one.
module Language.PlutusTx.Coordination.Contracts.PubKey(lock, lock') where

import           Control.Monad.Trans       (MonadTrans(lift))
import           Data.Maybe (listToMaybe)
import qualified Data.Map   as Map
import qualified Data.Text  as Text

import qualified Language.PlutusTx            as P
import           Ledger                       as Ledger hiding (initialise, to)
import           Ledger.Validation            as V
import           Wallet.API                   as WAPI

import           Language.PlutusTx.Cont

pkValidator :: PubKey -> ValidatorScript
pkValidator pk = 
    ValidatorScript (Ledger.applyScript mkValidator (Ledger.lifted pk)) where
        mkValidator = 
            Ledger.fromCompiledCode ($$(P.compile [||
                let 
                    validate :: PubKey -> () -> () -> PendingTx -> ()
                    validate pk' () () p =
                        if $$(V.txSignedBy) p pk'
                        then ()
                        else $$(P.error) ($$(P.traceH) "Required signature not present!" ())
                in validate
            ||]))

-- | Lock some funds in a 'PayToPubKey' contract, returning the output's address
--   and a 'TxIn' transaction input that can spend it.
lock :: (WalletAPI m, WalletDiagnostics m) => PubKey -> Value -> m (Address, TxIn)
lock pk vl = getRef =<< payToScript defaultSlotRange addr vl pkDataScript where
    addr = Ledger.scriptAddress (pkValidator pk)
    pkDataScript = DataScript $ Ledger.lifted ()
    pkRedeemer = RedeemerScript $ Ledger.lifted ()

    getRef tx = do
        let scriptOuts = listToMaybe
                            $ fmap fst
                            $ filter ((==) addr . txOutAddress . snd)
                            $ Map.toList (unspentOutputsTx tx)

        txin <- case scriptOuts of
                    Nothing -> throwOtherError
                                $ "transaction did not contain script output"
                                <> "for public key '"
                                <> Text.pack (show pk)
                                <> "'"
                    Just o  -> pure (scriptTxIn o (pkValidator pk) pkRedeemer)
            
        pure (addr, txin)

-- | A variant of 'lock' that runs in 'MonadWalletCont' and waits for the
--   transaction to be confirmed before returning.
lock' :: MonadWallet m => PubKey -> Value -> MonadWalletCont m TxIn
lock' pk vl = do
    (refAddr, refTxIn) <- lift (lock pk vl)
    await (fundsAtAddressT refAddr (WAPI.intervalFrom vl))
    pure refTxIn
