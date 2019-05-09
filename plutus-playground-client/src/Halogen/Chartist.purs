module Halogen.Chartist where

import Chartist (CHARTIST, Chart)
import Chartist as Chartist
import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Category ((<<<))
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import DOM (DOM)
import Data.Function (($))
import Data.Maybe (Maybe(Just, Nothing))
import Data.NaturalTransformation (type (~>))
import Data.Unit (Unit, unit)
import Halogen (RefLabel(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type ChartistState =
  { chart :: Maybe Chart
  }

data ChartistQuery a
  = Init a
  | SomeInput Int a

data ChartistMessage
  = Initialized

type HTML = H.ComponentHTML ChartistQuery

type DSL g = H.ComponentDSL ChartistState ChartistQuery ChartistMessage g

type ChartistEffects eff =
  ( chartist :: CHARTIST
  , dom :: DOM
  , exception :: EXCEPTION
  | eff
  )

chartist
  :: forall eff g.
  (MonadAff (ChartistEffects eff) g)
  => H.Component HH.HTML ChartistQuery Unit ChartistMessage g
chartist = H.lifecycleComponent
  { initialState: \_ -> { chart: Nothing }
  , render
  , eval
  , initializer: Just $ H.action Init
  , finalizer: Nothing
  , receiver: Just <<< SomeInput 5
  }

eval
  ∷ ∀ eff g
  . (MonadAff (ChartistEffects eff) g)
  ⇒ ChartistQuery ~> (DSL g)
eval (Init next) = do
  mElement <- H.getHTMLElementRef chartRefLabel
  case mElement of
    Nothing -> pure unit
    Just element -> do
      chart <- liftEff $ Chartist.barChart element
      H.modify _{ chart = Just chart }
      H.raise Initialized
  pure next

eval (SomeInput n next) = do
  pure next

chartRefLabel :: RefLabel
chartRefLabel = RefLabel "chartist"

render ∷ ChartistState → HTML
render state =
  HH.div
    [ HP.ref chartRefLabel ]
    []
