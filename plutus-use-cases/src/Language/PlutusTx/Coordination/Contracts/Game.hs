{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}

-- | A guessing game
module Language.PlutusTx.Coordination.Contracts.Game
    ( lock
    , guess
    , game
    , GameSchema
    , GuessParams(..)
    , LockParams(..)
    -- * Scripts
    , gameValidator
    , gameDataScript
    , gameRedeemerScript
    -- * Address
    , gameAddress
    , validateGuess
    -- * Traces
    , guessTrace
    , guessWrongTrace
    , lockTrace
    ) where

import Control.Monad (void)
import qualified Data.Aeson as Aeson
import GHC.Generics (Generic)
import IOTS (IotsType)
import Language.Plutus.Contract
import Language.Plutus.Contract.Schema ()
import Language.Plutus.Contract.Trace (ContractTrace, MonadEmulator, TraceError)
import qualified Language.Plutus.Contract.Trace as Trace
import qualified Language.PlutusTx as PlutusTx
import Language.PlutusTx.Prelude
import Ledger
    ( Ada
    , Address
    , DataScript
    , PendingTx
    , RedeemerScript
    , ValidatorScript
    )
import Ledger.Typed.Scripts (wrapValidator)
import Schema (ToSchema)

import qualified Ledger as Ledger
import qualified Ledger.Ada as Ada

import qualified Data.ByteString.Lazy.Char8 as C
import qualified Prelude

newtype HashedString = HashedString ByteString deriving newtype PlutusTx.IsData

PlutusTx.makeLift ''HashedString

newtype ClearString = ClearString ByteString deriving newtype PlutusTx.IsData

PlutusTx.makeLift ''ClearString

type GameSchema =
    BlockchainActions
        .\/ Endpoint "lock" LockParams
        .\/ Endpoint "guess" GuessParams

-- | The validator (datascript -> redeemer -> PendingTx -> Bool)
validateGuess :: HashedString -> ClearString -> PendingTx -> Bool
validateGuess (HashedString actual) (ClearString guess') _ = actual == sha2_256 guess'

gameValidator :: ValidatorScript
gameValidator = Ledger.mkValidatorScript $$(PlutusTx.compile [|| validator ||])
    where validator = wrapValidator validateGuess

-- create a data script for the guessing game by hashing the string
-- and lifting the hash to its on-chain representation
gameDataScript :: String -> DataScript
gameDataScript =
    Ledger.DataScript . PlutusTx.toData . HashedString . sha2_256 . C.pack

-- create a redeemer script for the guessing game by lifting the
-- string to its on-chain representation
gameRedeemerScript :: String -> RedeemerScript
gameRedeemerScript =
    Ledger.RedeemerScript . PlutusTx.toData . ClearString . C.pack

-- | The address of the game (the hash of its validator script)
gameAddress :: Address
gameAddress = Ledger.scriptAddress gameValidator

-- | Parameters for the "lock" endpoint
data LockParams = LockParams
    { secretWord :: String
    , amount     :: Ada
    }
    deriving stock (Prelude.Eq, Prelude.Ord, Prelude.Show, Generic)
    deriving anyclass (Aeson.FromJSON, Aeson.ToJSON, IotsType, ToSchema)

--  | Parameters for the "guess" endpoint
newtype GuessParams = GuessParams
    { guessWord :: String
    }
    deriving stock (Prelude.Eq, Prelude.Ord, Prelude.Show, Generic)
    deriving anyclass (Aeson.FromJSON, Aeson.ToJSON, IotsType, ToSchema)

guess :: Contract GameSchema e ()
guess = do
    GuessParams theGuess <- endpoint @"guess" @GuessParams
    mp <- utxoAt gameAddress
    let redeemer = gameRedeemerScript theGuess
        tx       = collectFromScript mp gameValidator redeemer
    void (writeTx tx)

lock :: Contract GameSchema e ()
lock = do
    LockParams secret amt <- endpoint @"lock" @LockParams
    let
        vl         = Ada.toValue amt
        dataScript = gameDataScript secret
        tx         = payToScript vl (Ledger.scriptAddress gameValidator) dataScript
    void (writeTx tx)

game :: Contract GameSchema e ()
game = guess <|> lock

lockTrace
    :: ( MonadEmulator (TraceError e) m )
    => ContractTrace GameSchema e m () ()
lockTrace =
    let w1 = Trace.Wallet 1 in
    Trace.callEndpoint @"lock" w1 (LockParams "secret" 10)
        >> Trace.handleBlockchainEvents w1

guessTrace
    :: ( MonadEmulator (TraceError e) m )
    => ContractTrace GameSchema e m () ()
guessTrace =
    let w2 = Trace.Wallet 2 in
    lockTrace
        >> Trace.callEndpoint @"guess" w2 (GuessParams "secret")
        >> Trace.handleBlockchainEvents w2

guessWrongTrace
    :: ( MonadEmulator (TraceError e) m )
    => ContractTrace GameSchema e m () ()
guessWrongTrace =
    let w2 = Trace.Wallet 2 in
    lockTrace
        >> Trace.callEndpoint @"guess" w2 (GuessParams "SECRET")
        >> Trace.handleBlockchainEvents w2
