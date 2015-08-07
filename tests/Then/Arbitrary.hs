{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Then.Arbitrary where

import           Control.Monad             (mzero)
import           Data.Aeson
import           Data.Char                 (chr)
import           Data.ByteString           (ByteString)
import           Data.CaseInsensitive      (CI (..), FoldCase, mk, original)
import           Network.HTTP.Types        (Header)
import           GHC.Generics              (Generic)
import           Test.QuickCheck

import qualified Data.ByteString.Char8     as BS
import qualified Data.Text                 as Text

import Then.Types

data RequestInfo = RequestInfo
    { path       :: ByteString
    , reqHeaders :: [Header]
    , method     :: String
    , body       :: ByteString
    } deriving (Eq, Read, Show, Generic)


newtype AsciiText = AsciiText { unAsciiText :: Text.Text }
  deriving (Eq, Show, Read, Generic)

-- * Instances

instance Arbitrary AsciiText where
    arbitrary = do
      str <- listOf . elements $ chr <$> [32..126]
      return $! AsciiText (Text.pack str)

instance Arbitrary LoginByUsername where
    arbitrary = LoginByUsername <$> arbitrary <*> arbitrary

instance Arbitrary LoginByEmail where
    arbitrary = LoginByEmail <$> arbitrary <*> arbitrary

instance FromJSON RequestInfo
instance ToJSON RequestInfo

instance ToJSON ByteString where
    toJSON bs = toJSON $ BS.unpack bs

instance FromJSON ByteString where
    parseJSON s@(String _) = BS.pack <$> parseJSON s
    parseJSON _            = mzero

instance ToJSON (CI ByteString) where
    toJSON ci = toJSON (original ci)

instance FromJSON (CI ByteString) where
    parseJSON s@(String _) = mk <$> parseJSON s
    parseJSON _            = mzero

instance (FoldCase a, Arbitrary a) => Arbitrary (CI a) where
    arbitrary = mk <$> arbitrary

instance Arbitrary ByteString where
    arbitrary = BS.pack <$> arbitrary

instance Arbitrary Text.Text where
    arbitrary = Text.pack <$> arbitrary
