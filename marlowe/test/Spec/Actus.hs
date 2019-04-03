{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns
-fno-warn-name-shadowing
-fno-warn-unused-do-bind
-fno-warn-unused-top-binds
-fno-warn-unused-imports #-}

module Spec.Actus
    ( tests
    )
where

import qualified Data.ByteString            as BS
import qualified Data.Map.Strict            as Map
import           Hedgehog                   (Property)
import           Test.Tasty
import           Test.Tasty.Hedgehog        (HedgehogTestLimit (..), testProperty)
import           Test.Tasty.HUnit

import           Language.Marlowe           hiding (discountFromPairList, insertCommit, mergeChoices)
import           Language.Marlowe.Actus     as Actus
import           Language.Marlowe.Client    (commit', evalContract, receivePayment, redeem)
import qualified Language.PlutusTx.Builtins as Builtins
import           Ledger                     hiding (Value)
import qualified Ledger.Ada                 as Ada
import           Spec.Common
import           Wallet.Emulator


tests :: TestTree
tests = testGroup "Actus"
        [ testCase "Safe zero coupon bond" checkZeroCouponBond
        , testCase "Trusted zero coupon bond" checkTrustedZeroCouponBond

        -- TODO: fix zero coupon bond tests and add them back in
        -- , localOption (HedgehogTestLimit $ Just 3) $
        --     testProperty "Safe zero coupon bond on mockchain" zeroCouponBondMockchainTest
        -- , localOption (HedgehogTestLimit $ Just 3) $
        --     testProperty "Safe zero coupon bond with guarantor on mockchain"
        --         zeroCouponBondGuaranteedMockchainTest
        -- , localOption (HedgehogTestLimit $ Just 3) $
            -- testProperty "Coupon bond" checkCouponBond
        ]

creatorPk, counterpartyPk, guarantorPk :: PubKey
creatorPk       = toPublicKey privateKey1
counterpartyPk  = toPublicKey privateKey2
guarantorPk     = toPublicKey privateKey3

testTxHash :: TxHash
testTxHash  = TxHash "12345678901234567890123456789012"

signature1, signature2 :: Signature
signature1  = sign ("12345678901234567890123456789012" :: BS.ByteString) privateKey1
signature2  = sign ("12345678901234567890123456789012" :: BS.ByteString) privateKey2

checkZeroCouponBond :: IO ()
checkZeroCouponBond = do
    let input cmd = Input cmd [] []
        state = State [] []
        notionalPrincipal = 1000
        premiumDiscount = 80
        initialExchangeDate = 50
        maturityDate = 500
        gracePeriod = 30240 -- about a week, 20sec * 3 * 60 * 24 * 7
        deposit = 12
        contract = zeroCouponBond creatorPk counterpartyPk notionalPrincipal premiumDiscount initialExchangeDate maturityDate gracePeriod
        eval = evalContract creatorPk testTxHash
    -- counterpartyID commits money for a bond with discount
    let (state1, con1, v) = eval (input $ Commit (IdentCC 1) signature2) (Slot 10)
                                    (Ada.fromInt deposit)
                                    (Ada.fromInt (notionalPrincipal - premiumDiscount + deposit))
                                    state
                                    contract
    v @?= True
    -- creatorID commits money for a bond redeem
    let (state2, con2, v) = eval (input $ Commit (IdentCC 2) signature1) (Slot 20)
                                    (Ada.fromInt (notionalPrincipal - premiumDiscount + deposit))
                                    (Ada.fromInt (2*notionalPrincipal - premiumDiscount + deposit))
                                    state1
                                    con1
    v @?= True
    -- creatorID receives payment for a bond
    let (state3, con3, v) = eval (input $ Payment (IdentPay 1) signature1) (Slot 60)
                                    (Ada.fromInt (2*notionalPrincipal - premiumDiscount + deposit))
                                    (Ada.fromInt (notionalPrincipal + deposit))
                                    state2
                                    con2
    v @?= True
    -- counterpartyID redeems a bond
    let (_, _, v) = eval (input $ Payment (IdentPay 2) signature2) (Slot 510)
                                    (Ada.fromInt (notionalPrincipal + deposit))
                                    (Ada.fromInt deposit)
                                    state3
                                    con3
    v @?= True
    -- creatorID can't receive payment for a bond before start date
    let (_, _, v) = eval (input $ Payment (IdentPay 1) signature1) (Slot 49)
                                    (Ada.fromInt (2*notionalPrincipal - premiumDiscount + deposit))
                                    (Ada.fromInt (notionalPrincipal + deposit))
                                    state2
                                    con2
    v @?= False


checkTrustedZeroCouponBond :: IO ()
checkTrustedZeroCouponBond = do
    let input cmd = Input cmd [] []
        state = State [] []
        notionalPrincipal = 1000
        premiumDiscount = 80
        initialExchangeDate = 50
        maturityDate = 500
        gracePeriod = 30240 -- about a week, 20sec * 3 * 60 * 24 * 7
        deposit = 12
        contract = trustedZeroCouponBond
                        creatorPk
                        counterpartyPk
                        notionalPrincipal
                        premiumDiscount
                        initialExchangeDate
                        maturityDate
                        gracePeriod
        eval = evalContract creatorPk testTxHash
    -- counterpartyID commits money for a bond with discount
    let (state1, con1, v) = eval (input $ Commit (IdentCC 1) signature2) (Slot 10)
                                    (Ada.fromInt deposit)
                                    (Ada.fromInt (notionalPrincipal - premiumDiscount + deposit))
                                    state
                                    contract
    v @?= True
    -- creatorID receives payment for a bond
    let (state2, con2, v) = eval (input $ Payment (IdentPay 1) signature1) (Slot 60)
                                    (Ada.fromInt (notionalPrincipal - premiumDiscount + deposit))
                                    (Ada.fromInt deposit)
                                    state1
                                    con1
    v @?= True
    -- creatorID commits money for a bond redeem
    let (state3, con3, v) = eval (input $ Commit (IdentCC 2) signature1) (Slot 450)
                                    (Ada.fromInt deposit)
                                    (Ada.fromInt (notionalPrincipal + deposit))
                                    state2
                                    con2
    v @?= True

    -- counterpartyID redeems a bond
    let (_, _, v) = eval (input $ Payment (IdentPay 2) signature2) (Slot 510)
                                    (Ada.fromInt (notionalPrincipal + deposit))
                                    (Ada.fromInt deposit)
                                    state3
                                    con3
    v @?= True
    -- creatorID can't receive payment for a bond before start date
    let (_, _, v) = eval (input $ Payment (IdentPay 1) signature1) (Slot 49)
                                    (Ada.fromInt (2*notionalPrincipal - premiumDiscount + deposit))
                                    (Ada.fromInt (notionalPrincipal + deposit))
                                    state1
                                    con1
    v @?= False


zeroCouponBondMockchainTest :: Property
zeroCouponBondMockchainTest = checkMarloweTrace (MarloweScenario {
    mlInitialBalances = Map.fromList [ (creatorPk, Ada.adaValueOf 1000000), (counterpartyPk, Ada.adaValueOf 1000000) ] }) $ do
    -- Init a contract
    let creatorID = Wallet 1
        counterpartyID = Wallet 2
        update = updateAll [creatorID, counterpartyID]
        notionalPrincipal = 1000
        premiumDiscount = 80
        initialExchangeDate = 50
        maturityDate = 500
        gracePeriod = 30240 -- about a week, 20sec * 3 * 60 * 24 * 7
    update

    let contract = zeroCouponBond creatorPk counterpartyPk notionalPrincipal premiumDiscount initialExchangeDate maturityDate gracePeriod

    withContract [creatorID, counterpartyID] contract $ \tx validator -> do
        tx <- counterpartyID `performs` commit'
            creatorPk
            tx
            validator
            [] []
            (IdentCC 1)
            (notionalPrincipal - premiumDiscount)
            emptyState
            contract

        update

        tx <- creatorID `performs` commit'
            creatorPk
            tx
            validator
            [] []
            (IdentCC 2)
            notionalPrincipal
            (State [ (IdentCC 1, (counterpartyPk, NotRedeemed (notionalPrincipal - premiumDiscount) maturityDate))] [])
            (CommitCash (IdentCC 2) creatorPk (Value notionalPrincipal) initialExchangeDate (maturityDate + gracePeriod)
                (When FalseObs initialExchangeDate Null
                    (Pay (IdentPay 1) counterpartyPk creatorPk (Committed (IdentCC 1)) maturityDate
                        (When FalseObs maturityDate Null
                            (Pay (IdentPay 2) creatorPk counterpartyPk (Committed (IdentCC 2))
                                (maturityDate + gracePeriod) Null)
                        )
                    )
                )
                Null
            )

        addBlocksAndNotify [creatorID, counterpartyID] (initialExchangeDate + 10)

        tx <- creatorID `performs` receivePayment tx
            validator
            [] []
            (IdentPay 1)
            (notionalPrincipal - premiumDiscount)
            (State [(IdentCC 2, (creatorPk, NotRedeemed notionalPrincipal (maturityDate + gracePeriod)))] [])
            (When FalseObs maturityDate Null
                (Pay (IdentPay 2) creatorPk counterpartyPk (Committed (IdentCC 2))
                    (maturityDate + gracePeriod) Null)
            )

        addBlocksAndNotify [creatorID, counterpartyID] maturityDate

        tx <- counterpartyID `performs` receivePayment tx
            validator
            [] []
            (IdentPay 2)
            notionalPrincipal
            (State [] [])
            Null

        return (tx, State [] [])
    assertOwnFundsEq creatorID (Ada.adaValueOf 999920)
    assertOwnFundsEq counterpartyID (Ada.adaValueOf 1000080)
    return ()


zeroCouponBondGuaranteedMockchainTest :: Property
zeroCouponBondGuaranteedMockchainTest = checkMarloweTrace (MarloweScenario {
    mlInitialBalances = Map.fromList    [ (creatorPk, Ada.adaValueOf 1000000)
                                        , (counterpartyPk, Ada.adaValueOf 1000000)
                                        , (guarantorPk, Ada.adaValueOf 1000000) ] }) $ do
    -- Init a contract
    let creatorID = Wallet 1
        creatorPk = creatorPk
        counterpartyID = Wallet 2
        counterpartyPk = counterpartyPk
        guarantor = Wallet 3
        update = updateAll [creatorID, counterpartyID, guarantor]
        notionalPrincipal = 1000
        premiumDiscount = 80
        initialExchangeDate = 50
        maturityDate = 500
        gracePeriod = 30240 -- about a week, 20sec * 3 * 60 * 24 * 7
    update

    let contract = zeroCouponBondGuaranteed
                        creatorPk counterpartyPk guarantorPk -- parties
                        notionalPrincipal premiumDiscount -- values
                        initialExchangeDate maturityDate gracePeriod -- dates

    withContract [creatorID, counterpartyID, guarantor] contract $ \tx validator -> do
        -- counterpartyID commits money for a bond with premiumDiscount
        tx <- counterpartyID `performs` commit'
            creatorPk
            tx
            validator
            [] []
            (IdentCC 1)
            (notionalPrincipal - premiumDiscount)
            emptyState
            contract

        update

        -- guarantor commits a guarantee
        tx <- guarantor `performs` commit'
            creatorPk
            tx
            validator
            [] []
            (IdentCC 2)
            notionalPrincipal
            (State [ (IdentCC 1, (counterpartyPk, NotRedeemed (notionalPrincipal - premiumDiscount) maturityDate))] [])
            (CommitCash (IdentCC 2) guarantorPk (Value notionalPrincipal) initialExchangeDate (maturityDate + gracePeriod)
                (When FalseObs initialExchangeDate Null
                    (Pay (IdentPay 1) counterpartyPk creatorPk (Committed (IdentCC 1)) maturityDate
                        (CommitCash (IdentCC 3) creatorPk (Value notionalPrincipal) maturityDate (maturityDate + gracePeriod)
                            -- if the creatorID commits the notionalPrincipal before maturity date pay from it, redeem the 'guarantee'
                            (Pay (IdentPay 2) creatorPk counterpartyPk (Committed (IdentCC 3))
                                (maturityDate + gracePeriod) (RedeemCC (IdentCC 2) Null))
                            -- pay from the guarantor otherwise
                            (Pay (IdentPay 3) guarantorPk counterpartyPk (Committed (IdentCC 2))
                                (maturityDate + gracePeriod) Null)
                        )
                    )
                )
                Null
            )

        addBlocksAndNotify [creatorID, counterpartyID, guarantor] (initialExchangeDate + 10)

        -- after initialExchangeDate the creatorID recevies the bond payment
        tx <- creatorID `performs` receivePayment tx
            validator
            [] []
            (IdentPay 1)
            (notionalPrincipal - premiumDiscount)
            (State [(IdentCC 2, (guarantorPk, NotRedeemed notionalPrincipal (maturityDate + gracePeriod)))] [])
            (CommitCash (IdentCC 3) creatorPk (Value notionalPrincipal) maturityDate (maturityDate + gracePeriod)
                -- if the creatorID commits the notionalPrincipal before maturity date pay from it, redeem the 'guarantee'
                (Pay (IdentPay 2) creatorPk counterpartyPk (Committed (IdentCC 3))
                    (maturityDate + gracePeriod) (RedeemCC (IdentCC 2) Null))
                -- pay from the guarantor otherwise
                (Pay (IdentPay 3) guarantorPk counterpartyPk (Committed (IdentCC 2))
                    (maturityDate + gracePeriod) Null)
            )

        addBlocksAndNotify [creatorID, counterpartyID, guarantor] 100

        -- before maturityDate the creatorID commits the bond value
        tx <- creatorID `performs` commit'
            creatorPk
            tx
            validator
            [] []
            (IdentCC 3)
            notionalPrincipal
            (State [(IdentCC 2, (guarantorPk, NotRedeemed notionalPrincipal (maturityDate + gracePeriod)))] [])
            (CommitCash (IdentCC 3) creatorPk (Value notionalPrincipal) maturityDate (maturityDate + gracePeriod)
                -- if the creatorID commits the notionalPrincipal before maturity date pay from it, redeem the 'guarantee'
                (Pay (IdentPay 2) creatorPk counterpartyPk (Committed (IdentCC 3))
                    (maturityDate + gracePeriod) (RedeemCC (IdentCC 2) Null))
                -- pay from the guarantor otherwise
                (Pay (IdentPay 3) guarantorPk counterpartyPk (Committed (IdentCC 2))
                    (maturityDate + gracePeriod) Null)
            )

        addBlocksAndNotify [creatorID, counterpartyID, guarantor] maturityDate

        -- after maturity date the counterpartyID collects the bond payment
        tx <- counterpartyID `performs` receivePayment tx
            validator
            [] []
            (IdentPay 2)
            notionalPrincipal
            (State  [ (IdentCC 2, (guarantorPk, NotRedeemed notionalPrincipal (maturityDate + gracePeriod)))] [])
            (RedeemCC (IdentCC 2) Null)

        update

        -- after that guarantor can recall the `guarantee` commit
        tx <- guarantor `performs` redeem
            tx
            validator
            [] []
            (IdentCC 2)
            notionalPrincipal
            (State [] [])
            Null

        return (tx, State [] [])

    assertOwnFundsEq creatorID (Ada.adaValueOf 999920)
    assertOwnFundsEq counterpartyID (Ada.adaValueOf 1000080)
    assertOwnFundsEq guarantor (Ada.adaValueOf 1000000)
    return ()

checkCouponBond :: Property
checkCouponBond = checkMarloweTrace (MarloweScenario {
    mlInitialBalances = Map.fromList    [ (creatorPk, Ada.adaValueOf 1000000)
                                        , (counterpartyPk, Ada.adaValueOf 1000000)
                                        , (guarantorPk, Ada.adaValueOf 1000000) ] }) $ do
    -- Init a contract
    let creatorID = Wallet 1
        creatorPk = creatorPk
        counterpartyID = Wallet 2
        counterpartyPk = counterpartyPk
        guarantor = Wallet 3
        guarantorPk = guarantorPk
        update = updateAll [creatorID, counterpartyID, guarantor]
        notionalPrincipal = 1000
        coupon = 80
        initialExchangeDate = 50
        maturityDate = 500
        gracePeriod = 30240 -- about a week, 20sec * 3 * 60 * 24 * 7
    update

    let contract = couponBondGuaranteed
            creatorPk counterpartyPk guarantorPk -- parties
            notionalPrincipal coupon -- value, coupon
            initialExchangeDate [100, 200, 300] maturityDate gracePeriod -- dates

    withContract [creatorID, counterpartyID, guarantor] contract $ \tx validator -> do
        -- counterpartyID commits money for a bond with premiumDiscount
        tx <- counterpartyID `performs` commit'
            creatorPk
            tx
            validator
            [] []
            (IdentCC 1)
            notionalPrincipal
            emptyState
            contract

        update

        -- guarantor commits a guarantee
        tx <- guarantor `performs` commit'
            creatorPk
            tx
            validator
            [] []
            (IdentCC 2)
            (notionalPrincipal + 3 * coupon)
            (State [ (IdentCC 1, (counterpartyPk, NotRedeemed notionalPrincipal maturityDate))] [])
            Null

        addBlocksAndNotify [creatorID, counterpartyID, guarantor] (initialExchangeDate + 10)

        -- after initialExchangeDate the creatorID recevies the bond payment
        tx <- creatorID `performs` receivePayment tx
            validator
            [] []
            (IdentPay 1)
            (notionalPrincipal)
            (State [(IdentCC 2, (guarantorPk, NotRedeemed notionalPrincipal (maturityDate + gracePeriod)))] [])
            (CommitCash (IdentCC 3) creatorPk (Value notionalPrincipal) maturityDate (maturityDate + gracePeriod)
                -- if the creatorID commits the notionalPrincipal before maturity date pay from it, redeem the 'guarantee'
                (Pay (IdentPay 2) creatorPk counterpartyPk (Committed (IdentCC 3))
                    (maturityDate + gracePeriod) (RedeemCC (IdentCC 2) Null))
                -- pay from the guarantor otherwise
                (Pay (IdentPay 3) guarantorPk counterpartyPk (Committed (IdentCC 2))
                    (maturityDate + gracePeriod) Null)
            )

        addBlocksAndNotify [creatorID, counterpartyID, guarantor] 100

        -- before maturityDate the creatorID commits the bond value
        tx <- creatorID `performs` commit'
            creatorPk
            tx
            validator
            [] []
            (IdentCC 3)
            notionalPrincipal
            (State [(IdentCC 2, (guarantorPk, NotRedeemed notionalPrincipal (maturityDate + gracePeriod)))] [])
            (CommitCash (IdentCC 3) creatorPk (Value notionalPrincipal) maturityDate (maturityDate + gracePeriod)
                -- if the creatorID commits the notionalPrincipal before maturity date pay from it, redeem the 'guarantee'
                (Pay (IdentPay 2) creatorPk counterpartyPk (Committed (IdentCC 3))
                    (maturityDate + gracePeriod) (RedeemCC (IdentCC 2) Null))
                -- pay from the guarantor otherwise
                (Pay (IdentPay 3) guarantorPk counterpartyPk (Committed (IdentCC 2))
                    (maturityDate + gracePeriod) Null)
            )

        addBlocksAndNotify [creatorID, counterpartyID, guarantor] maturityDate

        -- after maturity date the counterpartyID collects the bond payment
        tx <- counterpartyID `performs` receivePayment tx
            validator
            [] []
            (IdentPay 2)
            notionalPrincipal
            (State  [ (IdentCC 2, (guarantorPk, NotRedeemed notionalPrincipal (maturityDate + gracePeriod)))] [])
            (RedeemCC (IdentCC 2) Null)

        update

        -- after that guarantor can recall the `guarantee` commit
        tx <- guarantor `performs` redeem
            tx
            validator
            [] []
            (IdentCC 2)
            notionalPrincipal
            (State [] [])
            Null

        return (tx, State [] [])

    assertOwnFundsEq creatorID (Ada.adaValueOf 999920)
    assertOwnFundsEq counterpartyID (Ada.adaValueOf 1000080)
    assertOwnFundsEq guarantor (Ada.adaValueOf 1000000)
    return ()
