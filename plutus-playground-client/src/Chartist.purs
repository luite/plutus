module Chartist where

import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Uncurried (EffFn2, EffFn3, runEffFn2, runEffFn3)
import Control.Monad.Writer (WriterT)
import Control.Semigroupoid ((<<<))
import DOM (DOM)
import DOM.HTML.Types (HTMLElement)
import Data.Array as Array
import Data.Foldable (foldl)
import Data.Foreign (Foreign)
import Data.Function (($))
import Data.Maybe (maybe)
import Data.StrMap (StrMap)
import Data.Unit (Unit)

foreign import data Chart :: Type

foreign import data CHARTIST ∷ Effect

type ChartItem =
  { label :: String
  , points :: Array Number
  }

toChartistData :: Array ChartItem -> ChartistData
toChartistData xs = foldl reducer { labels: [], series: initialSeries } xs
  where
    initialSeries = Array.replicate n []
    n = maybe 0 (Array.length <<< _.points) $ Array.head xs

    reducer { labels, series } { label, points } =
      { labels: Array.snoc labels label
      , series: Array.zipWith Array.snoc series points
      }

type ChartistData =
    { labels :: Array String
    , series :: Array (Array Number)
    }

sampleData :: Array ChartItem
sampleData =
  [ { label: "Jan", points: [ 15.0, 3.0 ] }
  , { label: "Feb", points: [ 10.0, 8.0 ] }
  , { label: "Mar", points: [ 25.0, 8.0 ] }
  , { label: "Apr", points: [ 48.0, 8.0 ] }
  , { label: "May", points: [ 28.0, 8.0 ] }
  , { label: "Jun", points: [ 12.0, 8.0 ] }
  , { label: "Jul", points: [ 28.0, 8.0 ] }
  , { label: "Aug", points: [ 45.0, 6.0 ] }
  , { label: "Sep", points: [ 87.0, 6.0 ] }
  , { label: "Oct", points: [ 51.0, 6.0 ] }
  , { label: "Nov", points: [ 13.0, 3.0 ] }
  , { label: "Dec", points: [ 25.0, 3.0 ] }
  ]

type ChartistOptions =
  { seriesBarDistance :: Int
  , chartPadding ::
       { top :: Int
       , bottom :: Int
       , right :: Int
       , left :: Int
       }
  , plugins :: Array ChartistPlugin
  }

sampleOptions :: ChartistOptions
sampleOptions =
  { seriesBarDistance: 10
  , chartPadding:
      { top: 15
      , bottom: 30
      , right: 30
      , left: 30
      }
  , plugins: samplePlugins
  }

foreign import samplePlugins :: Array ChartistPlugin
foreign import sampleResponsiveOptions :: Foreign
foreign import sampleAxisTitleOptions :: Foreign
foreign import  data ChartistPlugin :: Type

foreign import _barChart :: forall eff. EffFn3 (dom ∷ DOM, chartist ∷ CHARTIST | eff) HTMLElement ChartistOptions Foreign Chart

foreign import _updateData :: forall eff. EffFn2 (dom ∷ DOM, chartist ∷ CHARTIST | eff) Chart ChartistData Unit

barChart ::
  forall eff.
  HTMLElement
  -> Eff (dom ∷ DOM, chartist ∷ CHARTIST, exception ∷ EXCEPTION | eff) Chart
barChart element = do
  chart <- runEffFn3 _barChart element sampleOptions sampleResponsiveOptions
  runEffFn2 _updateData chart $ toChartistData sampleData
  pure chart

newtype CommandsT (i ∷ # Effect) m a = CommandsT (WriterT (StrMap Foreign) m a)
