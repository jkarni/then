{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Then.LoginSpec (spec) where

import           Control.Monad.Trans.Either
import           Data.Aeson                 (decode)
import           Data.Maybe                 (fromJust)
import           Data.Either                (isRight)
import qualified Data.Text                  as Text
import           Servant
import           Test.Hspec
import           Test.QuickCheck            (property, (==>))

import           Then.Login
import           Then.Utils
import           Then.Types

import           Then.Arbitrary             (AsciiText(..))
import           Then.Test.Utils

spec :: Spec
spec = do
  loginByUsernameSpec

loginByUsernameSpec :: Spec
loginByUsernameSpec = beforeAll setupDB $ describe "loginByUsername" $ do

  context "the user doesn't exist" $ do

    it "returns a 400" $ \conn ->
      property $ \x -> loginByUsername conn x `shouldLeftSatisfy`
        ((== 400) . errHTTPCode)

    it "returns an error description" $ \conn ->
      property $ \x -> loginByUsername conn x `shouldLeftSatisfy`
        (\y -> (== [loginError]) . errors . fromJust . decode $ errBody y)

    it "has error status field" $ \conn ->
      property $ \x -> loginByUsername conn x `shouldLeftSatisfy`
        (\y -> (== Failure) . status . fromJust . decode $ errBody y)


  context "the user exists, but password is incorrect" $ do

    it "returns a 400" $ \conn -> property $ \name pwd pwd' email ->
      pwd /= pwd' && Text.length name <= 80 ==> do
        let u = LoginByUsername name pwd
        withUser conn (User name pwd' email) $ loginByUsername conn u `shouldLeftSatisfy`
          ((== 400) . errHTTPCode)

    it "returns an error description" $ \conn -> property $ \name pwd pwd' email -> do
      pwd /= pwd' && Text.length name <= 80 ==> do
        let u = LoginByUsername name pwd
        withUser conn (User name pwd' email) $ loginByUsername conn u `shouldLeftSatisfy`
          (\y -> (== [loginError]) . errors . fromJust . decode $ errBody y)

    it "has error status field" $ \conn -> property $ \name pwd pwd' email -> do
      pwd /= pwd' && Text.length name <= 80 ==> do
        let u = LoginByUsername name pwd
        withUser conn (User name pwd' email) $ loginByUsername conn u `shouldLeftSatisfy`
          (\y -> (== Failure) . status . fromJust . decode $ errBody y)

  context "the user exists, and the password is correct" $ do

    it "returns a Right" $ \conn -> property $ \(AsciiText name) (AsciiText pwd) (AsciiText email) ->
      Text.length name <= 80 ==> do
        let u = LoginByUsername name pwd
        withUser conn (User name pwd email) $ do
          x <- runEitherT $ loginByUsername conn u
          x `shouldSatisfy` isRight
