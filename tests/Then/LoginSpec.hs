module Then.LoginSpec (spec) where

import           Control.Monad.Trans.Either
import           Database.PostgreSQL.Simple
import           Servant
import           Test.Hspec
import           Test.QuickCheck

import           Then.Arbitrary ()
import           Then.Login

spec :: Spec
spec = do
  loginByUsernameSpec

loginByUsernameSpec :: Spec
loginByUsernameSpec = describe "loginByUsername" $ do
  let cinfo = defaultConnectInfo

  it "returns a 400 in case the user doesn't exist" $
    property $ \x -> loginByUsername cinfo x `shouldLeftSatisfy` ((== 400) . errHTTPCode)

shouldLeftReturn :: (Show e, Eq e) => EitherT e IO a -> e -> Expectation
shouldLeftReturn x exp
  = eitherT (`shouldBe` exp) (\_ -> expectationFailure "Expected Left, but got Right") x

shouldLeftSatisfy :: Show e => EitherT e IO a -> (e -> Bool) -> Expectation
shouldLeftSatisfy x p
  = eitherT (`shouldSatisfy` p) (\_ -> expectationFailure "Expected Left, but got Right") x
