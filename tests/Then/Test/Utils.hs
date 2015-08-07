module Then.Test.Utils where

import           Data.Monoid
import           Database.PostgreSQL.Simple (Connection, connectPostgreSQL)
import qualified Data.ByteString.Char8 as BS
import           Control.Monad.Trans.Either
import           System.Process (callCommand)
import           Test.Hspec

import           Then.Login
import           Then.Types

shouldLeftReturn :: (Show e, Eq e) => EitherT e IO a -> e -> Expectation
shouldLeftReturn x e
  = eitherT (`shouldBe` e) (\_ -> expectationFailure "Expected Left, but got Right") x

shouldLeftSatisfy :: Show e => EitherT e IO a -> (e -> Bool) -> Expectation
shouldLeftSatisfy x p
  = eitherT (`shouldSatisfy` p) (\_ -> expectationFailure "Expected Left, but got Right") x

createDB :: IO ()
createDB = callCommand $
    "dropdb --if-exists " <> testDBName <> " || true"
 <> " && createdb " <> testDBName
 <> " && psql --quiet --file=schema/then.sql " <> testDBName

testDBName :: String
testDBName = "test_thendb"

setupDB :: IO Connection
setupDB = createDB >> connectPostgreSQL (BS.pack $ "dbname=" ++ testDBName)

withUser :: Connection -> User -> IO a -> IO a
withUser conn usr action = createUser conn usr *> action <* removeUser conn usr
