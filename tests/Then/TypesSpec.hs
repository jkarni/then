{-# LANGUAGE OverloadedStrings #-}
module Then.TypesSpec (spec) where

import Test.Hspec
import Data.Aeson
import Data.Monoid
import qualified Data.ByteString.Lazy.UTF8 as BSL
import Test.QuickCheck

import Then.Types
import Then.Arbitrary ()

spec :: Spec
spec = do
  userCreationSpec

userCreationSpec :: Spec
userCreationSpec = describe "UserCreation" $ do

  it "decodes the example from authentication_api.rst" $ do
    eitherDecode userCreationExample `shouldBe` (Right . UserCreation $ User
            { username = "Anna Müller"
            , email    = "anna@example.org"
            , password = "EckVocUbs3"
            })

  it "eitherDecode (encode x) == Right x" $ do
    property $ \x -> eitherDecode (encode x) `shouldBe` Right (x :: UserCreation)

userCreationExample :: BSL.ByteString
userCreationExample = BSL.fromString $
    "{\"content_type\": \"adhocracy_core.resources.principal.IUser\","
 <> " \"data\": {"
 <> "      \"adhocracy_core.sheets.principal.IUserBasic\": {"
 <> "          \"name\": \"Anna Müller\"},"
 <> "      \"adhocracy_core.sheets.principal.IUserExtended\": {"
 <> "          \"email\": \"anna@example.org\"},"
 <> "      \"adhocracy_core.sheets.principal.IPasswordAuthentication\": {"
 <> "          \"password\": \"EckVocUbs3\"}}}"
