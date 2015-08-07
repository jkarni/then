module Then.LoginSpec (spec) where

import qualified Data.ByteString.Char8      as BS
import           Database.PostgreSQL.Simple
import           Servant
import           Test.Hspec
import           Test.QuickCheck

import           Then.Arbitrary ()
import           Then.Test.Utils
import           Then.Login

spec :: Spec
spec = do
  loginByUsernameSpec

loginByUsernameSpec :: Spec
loginByUsernameSpec = beforeAll setupDB $ describe "loginByUsername" $ do

  it "returns a 400 in case the user doesn't exist" $ \conn ->
    property $ \x -> loginByUsername conn x `shouldLeftSatisfy` ((== 400) . errHTTPCode)

setupDB :: IO Connection
setupDB = createDB >> connectPostgreSQL (BS.pack $ "dbname=" ++ testDBName)
