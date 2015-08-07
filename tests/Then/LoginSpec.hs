module Then.LoginSpec (spec) where

import           Data.Aeson                 (decode)
import qualified Data.ByteString.Char8      as BS
import           Database.PostgreSQL.Simple
import           Servant
import           Test.Hspec
import           Test.QuickCheck

import           Then.Login
import           Then.Utils

import           Then.Arbitrary ()
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
        ((== Just loginError) . decode . errBody)


setupDB :: IO Connection
setupDB = createDB >> connectPostgreSQL (BS.pack $ "dbname=" ++ testDBName)
