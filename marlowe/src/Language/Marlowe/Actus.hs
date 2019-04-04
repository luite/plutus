module Language.Marlowe.Actus where
import           Language.Marlowe
import           Wallet.API       (PubKey (..))


{-|
    A zero-coupon bond is a debt security that doesn't pay nominalInterestRate (a coupon)
    but is traded at a deep premiumDiscount, rendering profit at maturity
    when the bond is redeemed for its full face value.
-}
zeroCouponBond :: PubKey -> PubKey -> Int -> Int -> Timeout -> Timeout -> Timeout -> Contract
zeroCouponBond creatorID counterpartyID notionalPrincipal premiumDiscount initialExchangeDate maturityDate gracePeriod =
    -- prepare money for zero-coupon bond, before it could be used
    CommitCash (IdentCC 1) counterpartyID (Value (notionalPrincipal - premiumDiscount)) initialExchangeDate maturityDate
        (CommitCash (IdentCC 2) creatorID (Value notionalPrincipal) initialExchangeDate (maturityDate + gracePeriod)
            (When FalseObs initialExchangeDate Null
                (Pay (IdentPay 1) counterpartyID creatorID (Committed (IdentCC 1)) maturityDate
                    (When FalseObs maturityDate Null
                        (Pay (IdentPay 2) creatorID counterpartyID (Committed (IdentCC 2))
                            (maturityDate + gracePeriod) Null)
                    )
                )
            )
            Null
        )
        Null

{-|
    A zero-coupon bond is a debt security that doesn't pay nominalInterestRate (a coupon)
    but is traded at a @premiumDiscount@, rendering profit at @maturityDate@
    when the bond is redeemed for its full face @notionalPrincipal@ value.
    The @creatorID@ is not forced to commit before @initialExchangeDate@, hence it's a trusted bond,
    as the final payment can fail.
    If an @counterpartyID@ does not redeem the bond value during @gracePeriod@ after @maturityDate@
    the @creatorID@ can keep the value.
-}
trustedZeroCouponBond :: PubKey -> PubKey -> Int -> Int -> Timeout -> Timeout -> Timeout -> Contract
trustedZeroCouponBond creatorID counterpartyID notionalPrincipal premiumDiscount initialExchangeDate maturityDate gracePeriod =
    -- prepare money for zero-coupon bond, before it could be used
    -- if the creatorID won't pull the payment, counterpartyID can redeem the commit after maturityDate
    CommitCash (IdentCC 1) counterpartyID (Value (notionalPrincipal - premiumDiscount)) initialExchangeDate maturityDate
        (When FalseObs initialExchangeDate Null -- after initialExchangeDate
            -- creatorID can 'pull' the payment before maturityDate
            (Pay (IdentPay 1) counterpartyID creatorID (Committed (IdentCC 1)) maturityDate
                -- creatorID must commit a bond value before maturityDate
                -- creatorID can redeem committed value if the inverstor won't 'pull' the payment
                -- within gracePeriod after maturityDate
                (CommitCash (IdentCC 2) creatorID (Value notionalPrincipal) maturityDate (maturityDate + gracePeriod)
                    -- TODO: should we allow pay off only after maturity date?
                    -- Can we allow it earlier when the creatorID commmitted the pay off?
                    (When FalseObs maturityDate Null
                        (Pay (IdentPay 2) creatorID counterpartyID (Committed (IdentCC 2))
                            (maturityDate + gracePeriod) Null))
                    Null
                )
            )
        )
        Null

{-|
    Zero coupon bond with @guarantor@ party, who secures @creatorID@ payment with
    `guarantee` collateral.
-}
zeroCouponBondGuaranteed :: PubKey -> PubKey -> PubKey -> Int -> Int -> Timeout -> Timeout -> Timeout -> Contract
zeroCouponBondGuaranteed creatorID counterpartyID guarantor notionalPrincipal premiumDiscount initialExchangeDate maturityDate gracePeriod =
    -- prepare money for zero-coupon bond, before it could be used
    CommitCash (IdentCC 1) counterpartyID (Value (notionalPrincipal - premiumDiscount)) initialExchangeDate maturityDate
        -- guarantor commits a 'guarantee' before initialExchangeDate
        (CommitCash (IdentCC 2) guarantor (Value notionalPrincipal) initialExchangeDate (maturityDate + gracePeriod)
            (When FalseObs initialExchangeDate Null
                (Pay (IdentPay 1) counterpartyID creatorID (Committed (IdentCC 1)) maturityDate
                    (CommitCash (IdentCC 3) creatorID (Value notionalPrincipal) maturityDate (maturityDate + gracePeriod)
                        -- if the creatorID commits the notionalPrincipal before maturity date pay from it, redeem the 'guarantee'
                        (Pay (IdentPay 2) creatorID counterpartyID (Committed (IdentCC 3))
                            (maturityDate + gracePeriod) (RedeemCC (IdentCC 2) Null))
                        -- pay from the guarantor otherwise
                        (Pay (IdentPay 3) guarantor counterpartyID (Committed (IdentCC 2))
                            (maturityDate + gracePeriod) Null)
                    )
                )
            )
            Null
        )
        Null

{-|
    Generic zero coupon bond with @guarantor@ party, who secures @creatorID@ payment with
    `guarantee` collateral.
    Can be used as both _trusted_ bond, and as a bond with guarantor payments
-}
genericBond :: PubKey -> PubKey -> Int -> Int -> Timeout -> Timeout -> Timeout -> (Contract -> Contract) -> Contract -> Contract
genericBond creatorID counterpartyID notionalPrincipal premiumDiscount initialExchangeDate maturityDate gracePeriod guarantorCommit guarantorPay =
    -- prepare money for zero-coupon bond, before it could be used
    CommitCash (IdentCC 1) counterpartyID (Value (notionalPrincipal - premiumDiscount)) initialExchangeDate maturityDate
        -- guarantor commits a 'guarantee' before initialExchangeDate
        (guarantorCommit $
            When FalseObs initialExchangeDate Null
                (Pay (IdentPay 1) counterpartyID creatorID (Committed (IdentCC 1)) maturityDate
                    (CommitCash (IdentCC 3) creatorID (Value notionalPrincipal) maturityDate (maturityDate + gracePeriod)
                        -- if the creatorID commits the notionalPrincipal before maturity date pay from it, redeem the 'guarantee'
                        (Pay (IdentPay 2) creatorID counterpartyID (Committed (IdentCC 3))
                            (maturityDate + gracePeriod) (RedeemCC (IdentCC 2) Null))
                        -- pay from the guarantor otherwise
                        guarantorPay
                    )
                )
        )
        Null


trustedZeroCouponBond1 :: PubKey -> PubKey -> Int -> Int -> Timeout -> Timeout -> Timeout -> Contract
trustedZeroCouponBond1 creatorID counterpartyID notionalPrincipal premiumDiscount initialExchangeDate maturityDate gracePeriod = let
    guarantorCommit cont = cont
    in genericBond  creatorID counterpartyID
                    notionalPrincipal premiumDiscount
                    initialExchangeDate maturityDate gracePeriod
                    guarantorCommit
                    Null


zeroCouponBondGuaranteed1 :: PubKey -> PubKey -> PubKey -> Int -> Int -> Timeout -> Timeout -> Timeout -> Contract
zeroCouponBondGuaranteed1 creatorID counterpartyID guarantor notionalPrincipal premiumDiscount initialExchangeDate maturityDate gracePeriod = let
    guarantorCommit cont =
        CommitCash (IdentCC 2) guarantor (Value notionalPrincipal) initialExchangeDate (maturityDate + gracePeriod)
            cont
            Null
    guarantorPay = Pay (IdentPay 3) guarantor counterpartyID (Committed (IdentCC 2)) (maturityDate + gracePeriod) Null
    in genericBond  creatorID counterpartyID
                    notionalPrincipal premiumDiscount
                    initialExchangeDate maturityDate gracePeriod
                    guarantorCommit guarantorPay

{-
    Coupon bond with @guarantor@ party, who secures @creatorID@ payment with
    `guarantee` collateral.
    Issuer pays coupon on @paymentDates@
-}
couponBondGuaranteed :: PubKey
    -> PubKey
    -> PubKey
    -> Int
    -> Double
    -> Timeout
    -> Timeout
    -> Timeout
    -> Timeout
    -> Contract
couponBondGuaranteed creatorID counterpartyID guarantor notionalPrincipal nominalInterestRate initialExchangeDate slotCycle maturityDate gracePeriod =
    -- counterpartyID commits a bond face value before initialExchangeDate
    CommitCash (IdentCC 0) counterpartyID (Value notionalPrincipal) initialExchangeDate maturityDate
        -- guarantor commits a 'guarantee' before initialExchangeDate
        (CommitCash (IdentCC 1) guarantor (Value totalPayment) initialExchangeDate (maturityDate + gracePeriod)
            (Both
                -- creatorID can receive the payment from counterpartyID
                (Pay (IdentPay 1) counterpartyID creatorID (Committed (IdentCC 0)) maturityDate Null)
                -- schedule payments
                (Both payments finalPayment)
            )
            -- if no guarantee committed we abort contract and allow to redeem the counterpartyID's commit
            (RedeemCC (IdentCC 0) Null)
        )
        Null
  where
    cycles = takeWhile (\i ->
            let paymentDate = initialExchangeDate + i * slotCycle
            in paymentDate < maturityDate
        ) [1..]

    -- calculate payment schedule
    paymentDates = map (\i -> initialExchangeDate + i * slotCycle) cycles

    coupon = round $ fromIntegral notionalPrincipal * nominalInterestRate

    -- calculate total amount of payments to be ensured by guarantor
    totalPayment = notionalPrincipal + coupon * length cycles

    -- generate Commit/Pay for each scheduled payment
    payment amount (ident, paymentDate) =
        -- creatorID commits a coupon payment
        CommitCash (IdentCC ident) creatorID (Value amount) paymentDate (maturityDate + gracePeriod)
            (When FalseObs paymentDate Null
                -- counterpartyID can claim the coupon after payment date
                (Pay (IdentPay ident) creatorID counterpartyID (Committed (IdentCC ident)) (maturityDate + gracePeriod) Null))
            -- in case creatorID did not commit on time the guarantor pays the coupon
            (Pay (IdentPay (ident + 1)) guarantor counterpartyID (Value amount) (maturityDate + gracePeriod) Null)

    -- generate coupon payments for given schedule
    payments = foldr1 Both $ map (payment coupon) idsAndDates
        -- generate IdentCC/IdentPay identifiers for each payment date
        where idsAndDates = zip (map (2*) [1..]) paymentDates

    finalPayment = payment notionalPrincipal (2 * (1 + length paymentDates), maturityDate)
