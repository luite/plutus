module Language.Marlowe.Actus where
import           Language.Marlowe
import           Wallet.API       (PubKey (..))


{-|
    A zero-coupon bond is a debt security that doesn't pay interest (a coupon)
    but is traded at a deep discount, rendering profit at maturity
    when the bond is redeemed for its full face value.
-}
zeroCouponBond :: PubKey -> PubKey -> Int -> Int -> Timeout -> Timeout -> Timeout -> Contract
zeroCouponBond issuer investor notional discount startDate maturityDate gracePeriod =
    -- prepare money for zero-coupon bond, before it could be used
    CommitCash (IdentCC 1) investor (Value (notional - discount)) startDate maturityDate
        (CommitCash (IdentCC 2) issuer (Value notional) startDate (maturityDate + gracePeriod)
            (When FalseObs startDate Null
                (Pay (IdentPay 1) investor issuer (Committed (IdentCC 1)) maturityDate
                    (When FalseObs maturityDate Null
                        (Pay (IdentPay 2) issuer investor (Committed (IdentCC 2))
                            (maturityDate + gracePeriod) Null)
                    )
                )
            )
            Null
        )
        Null

{-|
    A zero-coupon bond is a debt security that doesn't pay interest (a coupon)
    but is traded at a @discount@, rendering profit at @maturityDate@
    when the bond is redeemed for its full face @notional@ value.
    The @issuer@ is not forced to commit before @startDate@, hence it's a trusted bond,
    as the final payment can fail.
    If an @investor@ does not redeem the bond value during @gracePeriod@ after @maturityDate@
    the @issuer@ can keep the value.
-}
trustedZeroCouponBond :: PubKey -> PubKey -> Int -> Int -> Timeout -> Timeout -> Timeout -> Contract
trustedZeroCouponBond issuer investor notional discount startDate maturityDate gracePeriod =
    -- prepare money for zero-coupon bond, before it could be used
    -- if the issuer won't pull the payment, investor can redeem the commit after maturityDate
    CommitCash (IdentCC 1) investor (Value (notional - discount)) startDate maturityDate
        (When FalseObs startDate Null -- after startDate
            -- issuer can 'pull' the payment before maturityDate
            (Pay (IdentPay 1) investor issuer (Committed (IdentCC 1)) maturityDate
                -- issuer must commit a bond value before maturityDate
                -- issuer can redeem committed value if the inverstor won't 'pull' the payment
                -- within gracePeriod after maturityDate
                (CommitCash (IdentCC 2) issuer (Value notional) maturityDate (maturityDate + gracePeriod)
                    -- TODO: should we allow pay off only after maturity date?
                    -- Can we allow it earlier when the issuer commmitted the pay off?
                    (When FalseObs maturityDate Null
                        (Pay (IdentPay 2) issuer investor (Committed (IdentCC 2))
                            (maturityDate + gracePeriod) Null))
                    Null
                )
            )
        )
        Null

{-|
    Zero coupon bond with @guarantor@ party, who secures @issuer@ payment with
    `guarantee` collateral.
-}
zeroCouponBondGuaranteed :: PubKey -> PubKey -> PubKey -> Int -> Int -> Timeout -> Timeout -> Timeout -> Contract
zeroCouponBondGuaranteed issuer investor guarantor notional discount startDate maturityDate gracePeriod =
    -- prepare money for zero-coupon bond, before it could be used
    CommitCash (IdentCC 1) investor (Value (notional - discount)) startDate maturityDate
        -- guarantor commits a 'guarantee' before startDate
        (CommitCash (IdentCC 2) guarantor (Value notional) startDate (maturityDate + gracePeriod)
            (When FalseObs startDate Null
                (Pay (IdentPay 1) investor issuer (Committed (IdentCC 1)) maturityDate
                    (CommitCash (IdentCC 3) issuer (Value notional) maturityDate (maturityDate + gracePeriod)
                        -- if the issuer commits the notional before maturity date pay from it, redeem the 'guarantee'
                        (Pay (IdentPay 2) issuer investor (Committed (IdentCC 3))
                            (maturityDate + gracePeriod) (RedeemCC (IdentCC 2) Null))
                        -- pay from the guarantor otherwise
                        (Pay (IdentPay 3) guarantor investor (Committed (IdentCC 2))
                            (maturityDate + gracePeriod) Null)
                    )
                )
            )
            Null
        )
        Null

{-|
    Generic zero coupon bond with @guarantor@ party, who secures @issuer@ payment with
    `guarantee` collateral.
    Can be used as both _trusted_ bond, and as a bond with guarantor payments
-}
genericBond :: PubKey -> PubKey -> Int -> Int -> Timeout -> Timeout -> Timeout -> (Contract -> Contract) -> Contract -> Contract
genericBond issuer investor notional discount startDate maturityDate gracePeriod guarantorCommit guarantorPay =
    -- prepare money for zero-coupon bond, before it could be used
    CommitCash (IdentCC 1) investor (Value (notional - discount)) startDate maturityDate
        -- guarantor commits a 'guarantee' before startDate
        (guarantorCommit $
            When FalseObs startDate Null
                (Pay (IdentPay 1) investor issuer (Committed (IdentCC 1)) maturityDate
                    (CommitCash (IdentCC 3) issuer (Value notional) maturityDate (maturityDate + gracePeriod)
                        -- if the issuer commits the notional before maturity date pay from it, redeem the 'guarantee'
                        (Pay (IdentPay 2) issuer investor (Committed (IdentCC 3))
                            (maturityDate + gracePeriod) (RedeemCC (IdentCC 2) Null))
                        -- pay from the guarantor otherwise
                        guarantorPay
                    )
                )
        )
        Null


trustedZeroCouponBond1 :: PubKey -> PubKey -> Int -> Int -> Timeout -> Timeout -> Timeout -> Contract
trustedZeroCouponBond1 issuer investor notional discount startDate maturityDate gracePeriod = let
    guarantorCommit cont = cont
    in genericBond  issuer investor
                    notional discount
                    startDate maturityDate gracePeriod
                    guarantorCommit
                    Null


zeroCouponBondGuaranteed1 :: PubKey -> PubKey -> PubKey -> Int -> Int -> Timeout -> Timeout -> Timeout -> Contract
zeroCouponBondGuaranteed1 issuer investor guarantor notional discount startDate maturityDate gracePeriod = let
    guarantorCommit cont =
        CommitCash (IdentCC 2) guarantor (Value notional) startDate (maturityDate + gracePeriod)
            cont
            Null
    guarantorPay = Pay (IdentPay 3) guarantor investor (Committed (IdentCC 2)) (maturityDate + gracePeriod) Null
    in genericBond  issuer investor
                    notional discount
                    startDate maturityDate gracePeriod
                    guarantorCommit guarantorPay



{-
    Coupon bond with @guarantor@ party, who secures @issuer@ payment with
    `guarantee` collateral.
    Issuer pays coupon on @paymentDates@
-}
couponBondGuaranteed :: PubKey
    -> PubKey
    -> PubKey
    -> Int
    -> Int
    -> Timeout
    -> [Timeout]
    -> Timeout
    -> Timeout
    -> Contract
couponBondGuaranteed issuer investor guarantor notional interest startDate paymentDates maturityDate gracePeriod =
    -- investor commits a bond face value before startDate
    CommitCash (IdentCC 0) investor (Value notional) startDate maturityDate
        -- guarantor commits a 'guarantee' before startDate
        (CommitCash (IdentCC 1) guarantor (Value totalPayment) startDate (maturityDate + gracePeriod)
            (Both
                -- issuer can receive the payment from investor
                (Pay (IdentPay 1) investor issuer (Committed (IdentCC 0)) maturityDate Null)
                -- schedule payments
                (Both payments finalPayment)
            )
            -- if no guarantee committed we abort contract and allow to redeem the investor's commit
            (RedeemCC (IdentCC 0) Null)
        )
        Null
  where
    numPayments = length paymentDates
    coupon = interest
    totalPayment = notional + interest * numPayments

    -- generate Commit/Pay for each scheduled payment
    payment amount (ident, paymentDate) =
        -- issuer commits a coupon payment
        CommitCash (IdentCC ident) issuer (Value amount) paymentDate (maturityDate + gracePeriod)
            (When FalseObs paymentDate Null
                -- investor can claim the coupon after payment date
                (Pay (IdentPay ident) issuer investor (Committed (IdentCC ident)) (maturityDate + gracePeriod) Null))
            -- in case issuer did not commit on time the guarantor pays the coupon
            (Pay (IdentPay (ident + 1)) guarantor investor (Value amount) (maturityDate + gracePeriod) Null)

    payments = foldr1 Both $ map (payment coupon) idsAndDates
        -- generate IdentCC/IdentPay identifiers for each payment date
        where idsAndDates = zip (map (2*) [1..]) paymentDates

    finalPayment = payment notional (2 * (1 + length paymentDates), maturityDate)
