module Main where

import Prelude

import Control.Coroutine (Consumer, Process, connect, consumer, runProcess)
import Effect.Aff (forkAff, Aff)
import Effect.Console (log)
import Effect.Class (liftEffect)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Control.Monad.Reader.Trans (runReaderT)
import Data.Maybe (Maybe(..), fromMaybe)
import Gist (GistId)
import Halogen (hoist)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import LocalStorage (RawStorageEvent)
import MainFrame (mainFrame)
import Meadow (SPParams_(SPParams_))
import Servant.PureScript.Settings
  ( SPSettingsToUrlPiece_(..)
  , SPSettings_(..)
  , URLPiece
  , defaultSettings
  , gDefaultToURLPiece
  )
import Type.Proxy (Proxy(..))

import LocalStorage as LocalStorage

ajaxSettings :: SPSettings_ SPParams_
ajaxSettings = SPSettings_ $ settings
  where
  SPSettings_ settings = defaultSettings $ SPParams_ { baseURL: "/api/"
                                                     }
main ::
  Effect Unit
main = runHalogenAff do
  body <- awaitBody
  driver <- runUI (hoist (flip runReaderT ajaxSettings) mainFrame) unit body
  forkAff $ runProcess watchLocalStorageProcess

watchLocalStorageProcess :: Process Aff Unit
watchLocalStorageProcess = connect LocalStorage.listen watchLocalStorage

watchLocalStorage ::
  forall r.
  Consumer RawStorageEvent Aff r
watchLocalStorage = consumer \event ->
  do
    liftEffect $ log $ "Got Local Storage Event: " <> show event
    pure Nothing

onLoad :: Unit
onLoad = unsafePerformEffect main
