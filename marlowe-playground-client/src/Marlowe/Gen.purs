module Marlowe.Gen where

import Prelude
import Control.Lazy (class Lazy, defer)
import Control.Monad.Gen (class MonadGen, chooseInt, resize, suchThat, unfoldable)
import Control.Monad.Gen as Gen
import Control.Monad.Reader (class MonadAsk, ask)
import Control.Monad.Rec.Class (class MonadRec)
import Data.BigInteger (BigInteger)
import Data.BigInteger as BigInteger
import Data.Char.Gen (genAlpha, genDigitChar)
import Data.Foldable (class Foldable)
import Data.NonEmpty (NonEmpty, foldl1, (:|))
import Data.String.CodeUnits (fromCharArray)
import Marlowe.Holes (AccountId(..), Action(..), Case(..), ChoiceId(..), Contract(..), Observation(..), Payee(..), Term(..), Value(..), ValueId(..), Bound(..))
import Marlowe.Semantics (PubKey, Slot(..), SlotInterval(..), Timeout)
import Text.Parsing.Parser.Pos (Position(..))
import Type.Proxy (Proxy(..))

oneOf ::
  forall m a f.
  Foldable f =>
  MonadGen m =>
  NonEmpty f (m a) ->
  m a
oneOf = foldl1 Gen.choose

genBigInteger :: forall m. MonadGen m => MonadRec m => m BigInteger
genBigInteger = BigInteger.fromInt <$> chooseInt bottom top

genSlot :: forall m. MonadGen m => MonadRec m => m Slot
genSlot = Slot <$> genBigInteger

genTimeout :: forall m. MonadGen m => MonadRec m => m Timeout
genTimeout = genSlot

genValueId :: forall m. MonadGen m => MonadRec m => MonadAsk Boolean m => m ValueId
genValueId = ValueId <$> genString

genAlphaNum :: forall m. MonadGen m => MonadRec m => m Char
genAlphaNum = oneOf $ genAlpha :| [ genDigitChar ]

genString :: forall m. MonadGen m => MonadRec m => m String
genString = fromCharArray <$> resize (_ - 1) (unfoldable genAlphaNum)

genPubKey :: forall m. MonadGen m => MonadRec m => m PubKey
genPubKey = genString

genSlotInterval :: forall m. MonadGen m => MonadRec m => m Slot -> m SlotInterval
genSlotInterval gen = do
  from <- gen
  to <- suchThat gen (\v -> v > from)
  pure $ SlotInterval from to

genBound :: forall m. MonadGen m => MonadRec m => MonadAsk Boolean m => m Bound
genBound = do
  from <- genBigInteger
  from' <- genTerm $ pure from
  to <- genTerm $ suchThat genBigInteger (\v -> v > from)
  pure $ Bound from' to

genPosition :: forall m. MonadGen m => MonadRec m => m Position
genPosition = do
  column <- chooseInt 0 1000
  line <- chooseInt 0 1000
  pure $ Position { column, line }

genHole :: forall m a. MonadGen m => MonadRec m => m (Term a)
genHole = do
  name <- suchThat genString (\s -> s /= "")
  proxy <- pure (Proxy :: Proxy a)
  start <- genPosition
  end <- genPosition
  pure $ Hole name proxy start end

genTerm :: forall m a. MonadGen m => MonadRec m => MonadAsk Boolean m => m a -> m (Term a)
genTerm g = do
  withHoles <- ask
  oneOf $ (Term <$> g) :| (if withHoles then [ genHole ] else [])

genAccountId :: forall m. MonadGen m => MonadRec m => MonadAsk Boolean m => m AccountId
genAccountId = do
  accountNumber <- genTerm genBigInteger
  accountOwner <- genTerm genPubKey
  pure $ AccountId accountNumber accountOwner

genChoiceId :: forall m. MonadGen m => MonadRec m => MonadAsk Boolean m => m ChoiceId
genChoiceId = do
  choiceName <- genTerm genString
  choiceOwner <- genTerm genPubKey
  pure $ ChoiceId choiceName choiceOwner

genPayee :: forall m. MonadGen m => MonadRec m => MonadAsk Boolean m => m Payee
genPayee = oneOf $ (Account <$> genTerm genAccountId) :| [ Party <$> genTerm genPubKey ]

genAction :: forall m. MonadGen m => MonadRec m => Lazy (m Observation) => Lazy (m Value) => MonadAsk Boolean m => Int -> m Action
genAction size =
  oneOf
    $ (Deposit <$> genTerm genAccountId <*> genTerm genPubKey <*> genTerm (genValue' size))
    :| [ Choice <$> genTerm genChoiceId <*> resize (_ - 1) (unfoldable (genTerm genBound))
      , Notify <$> genTerm (genObservation' size)
      ]

genCase ::
  forall m.
  MonadGen m =>
  MonadRec m =>
  Lazy (m Value) =>
  Lazy (m Observation) =>
  Lazy (m Contract) =>
  MonadAsk Boolean m =>
  Int ->
  m Case
genCase size = do
  let
    newSize = size - 1
  action <- genTerm $ genAction newSize
  contract <- genTerm $ genContract' newSize
  pure (Case action contract)

genCases ::
  forall m.
  MonadGen m =>
  MonadRec m =>
  Lazy (m Value) =>
  Lazy (m Observation) =>
  Lazy (m Contract) =>
  MonadAsk Boolean m =>
  Int ->
  m (Array (Term Case))
genCases size = resize (_ - 1) (unfoldable (genTerm (genCase size)))

genValue :: forall m. MonadGen m => MonadRec m => Lazy (m Value) => MonadAsk Boolean m => m Value
genValue = genValue' 5

genValue' ::
  forall m.
  MonadGen m =>
  MonadRec m =>
  Lazy (m Value) =>
  MonadAsk Boolean m =>
  Int ->
  m Value
genValue' size
  | size > 1 =
    defer \_ ->
      let
        newSize = (size - 1)

        genNewValue = genTerm $ genValue' newSize
      in
        oneOf $ pure SlotIntervalStart
          :| [ pure SlotIntervalEnd
            , AvailableMoney <$> genTerm genAccountId
            , Constant <$> genTerm genBigInteger
            , NegValue <$> genNewValue
            , AddValue <$> genNewValue <*> genNewValue
            , SubValue <$> genNewValue <*> genNewValue
            , ChoiceValue <$> genTerm genChoiceId <*> genNewValue
            , UseValue <$> genTerm genValueId
            ]
  | otherwise =
    oneOf $ pure SlotIntervalStart
      :| [ pure SlotIntervalEnd
        , AvailableMoney <$> genTerm genAccountId
        , Constant <$> genTerm genBigInteger
        , UseValue <$> genTerm genValueId
        ]

genObservation ::
  forall m.
  MonadGen m =>
  MonadRec m =>
  Lazy (m Observation) =>
  Lazy (m Value) =>
  MonadAsk Boolean m =>
  m Observation
genObservation = genObservation' 5

genObservation' ::
  forall m.
  MonadGen m =>
  MonadRec m =>
  Lazy (m Observation) =>
  Lazy (m Value) =>
  MonadAsk Boolean m =>
  Int ->
  m Observation
genObservation' size
  | size > 1 =
    defer \_ ->
      let
        newSize = (size - 1)

        genNewValue = genTerm $ genValue' newSize

        genNewObservation = genTerm $ genObservation' newSize
      in
        oneOf
          $ (AndObs <$> genNewObservation <*> genNewObservation)
          :| [ OrObs <$> genNewObservation <*> genNewObservation
            , NotObs <$> genNewObservation
            , ChoseSomething <$> genTerm genChoiceId
            , ValueGE <$> genNewValue <*> genNewValue
            , ValueGT <$> genNewValue <*> genNewValue
            , ValueLT <$> genNewValue <*> genNewValue
            , ValueLE <$> genNewValue <*> genNewValue
            , ValueEQ <$> genNewValue <*> genNewValue
            ]
  | otherwise = genLeaf
    where
    genLeaf ::
      m Observation
    genLeaf = ChoseSomething <$> genTerm genChoiceId

genContract ::
  forall m.
  MonadGen m =>
  MonadRec m =>
  Lazy (m Contract) =>
  Lazy (m Observation) =>
  Lazy (m Value) =>
  MonadAsk Boolean m =>
  m Contract
genContract = genContract' 3

genContract' ::
  forall m.
  MonadGen m =>
  MonadRec m =>
  Lazy (m Contract) =>
  Lazy (m Observation) =>
  Lazy (m Value) =>
  MonadAsk Boolean m =>
  Int ->
  m Contract
genContract' size
  | size > 1 =
    defer \_ ->
      let
        newSize = (size - 1)

        genNewValue = genTerm $ genValue' newSize

        genNewObservation = genTerm $ genObservation' newSize

        genNewContract = genTerm $ genContract' newSize
      in
        oneOf $ pure Close
          :| [ Pay <$> genTerm genAccountId <*> genTerm genPayee <*> genNewValue <*> genNewContract
            , If <$> genNewObservation <*> genNewContract <*> genNewContract
            , When <$> genCases newSize <*> genTerm genTimeout <*> genNewContract
            , Let <$> genTerm genValueId <*> genNewValue <*> genNewContract
            ]
  | otherwise = genLeaf
    where
    genLeaf ::
      m Contract
    genLeaf = pure Close
