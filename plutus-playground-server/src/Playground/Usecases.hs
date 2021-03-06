{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Playground.Usecases where

import           Data.FileEmbed     (embedFile, makeRelativeToProject)
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T

marker :: T.Text
marker = "TRIM TO HERE"

strip :: T.Text -> T.Text
strip text = snd $ T.breakOnEnd marker text

vesting :: T.Text
vesting = strip $ T.decodeUtf8 $(makeRelativeToProject "usecases/Vesting.hs" >>= embedFile)

game :: T.Text
game = strip $ T.decodeUtf8 $(makeRelativeToProject "usecases/Game.hs" >>= embedFile)

errorHandling :: T.Text
errorHandling = strip $ T.decodeUtf8 $(makeRelativeToProject "usecases/ErrorHandling.hs" >>= embedFile)

crowdfunding :: T.Text
crowdfunding = strip $ T.decodeUtf8 $(makeRelativeToProject "usecases/CrowdFunding.hs" >>= embedFile)

starter :: T.Text
starter = strip $ T.decodeUtf8 $(makeRelativeToProject "usecases/Starter.hs" >>= embedFile)
