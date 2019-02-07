{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Auth.TypesSpec
  ( spec
  ) where

import           Auth.Types           (OAuthToken (OAuthToken), Token (Token), TokenProvider (Github),
                                       oAuthTokenAccessToken, oAuthTokenScope, oAuthTokenTokenType)
import qualified Bazel.Runfiles       as Runfiles
import qualified Control.Exception    as E
import           Data.Aeson           (eitherDecode)
import qualified Data.ByteString.Lazy as LBS
import           Test.Hspec           (Spec, describe, it, shouldBe)

spec :: Spec
spec = oAuthTokenJsonHandlingSpec

oAuthTokenJsonHandlingSpec :: Spec
oAuthTokenJsonHandlingSpec =
  describe "OAuthToken JSON handling" $
  it "should be able to parse a github response" $ do
    file <- dataFile
    input1 <- LBS.readFile file
    let decoded :: Either String (OAuthToken 'Github) = eitherDecode input1
    decoded `shouldBe`
      Right
        (OAuthToken
           { oAuthTokenAccessToken =
               Token "ee3d47a281b8540a2d970a4bf5eb93cdaeb0ed52"
           , oAuthTokenScope = "gist"
           , oAuthTokenTokenType = "bearer"
           })

dataFile :: IO String
dataFile = do
   mr <-
       E.catch
           (Just <$> Runfiles.create)
           (\(_ :: E.SomeException) -> pure Nothing)
   case mr of
       Just r  -> pure $ Runfiles.rlocation r "plutus/plutus-playground/plutus-playground-server/test/oAuthToken1.json"
       Nothing -> pure "test/oAuthToken1.json"
