{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Then.Types (
    Error(..)
  , ErrorResponse(..)
  , LoginByUsername(..)
  , LoginByEmail(..)
  , LoginResult(..)
  , Status(..)
  , User(..)
  , newToken
  ) where

import           Control.Monad
import qualified Crypto.Nonce as Nonce
import           Data.Aeson                         hiding (Success)
import qualified Data.ByteString                    as BS
import qualified Data.ByteString.Char8              as BSC
import qualified Data.Text                          as Text
import           System.IO.Unsafe (unsafePerformIO)
import           GHC.Generics                       (Generic)
import           Database.PostgreSQL.Simple.FromRow (FromRow (..), field)


-- * Errors

data Error = Error { location    :: Text.Text
                   , description :: Text.Text
                   , name        :: Text.Text
                   } deriving (Eq, Show, Read, Generic, FromJSON, ToJSON)

data ErrorResponse = ErrorResponse { status :: Status
                                   , errors :: [Error]
                                   } deriving (Eq, Show, Read, Generic, FromJSON, ToJSON)



-- * Login

data LoginByUsername = LoginByUsername
    { loginByUsernameName     :: Text.Text
    , loginByUsernamePassword :: Text.Text
    } deriving (Eq, Generic, Show)

instance FromJSON LoginByUsername where
    parseJSON (Object v) = LoginByUsername <$> v .: "name"
                                           <*> v .: "password"

data LoginByEmail = LoginByEmail
    { loginByEmailName     :: Text.Text
    , loginByEmailPassword :: Text.Text
    } deriving (Eq, Generic, Show)

instance FromJSON LoginByEmail where
    parseJSON (Object v) = LoginByEmail <$> v .: "email"
                                        <*> v .: "password"


data Status = Success | Failure
    deriving (Eq, Show, Generic, Read)

instance FromJSON Status where
    parseJSON (String "success") = return Success
    parseJSON (String "error")   = return Failure
    parseJSON _                  = mzero

instance ToJSON Status where
    toJSON Success = String "success"
    toJSON Failure = String "error"


data LoginResult = LoginResult
    { loginResultStatus   :: Status
    , loginResultUserPath :: Text.Text
    , loginResultToken    :: Token
    } deriving (Eq, Show, Generic)

instance ToJSON LoginResult where
    toJSON (LoginResult status path token) = object [ "status" .= status
                                                    , "user_path" .= path
                                                    , "user_token" .= token
                                                    ]

-- * Token

newtype Token = Token { unToken :: BS.ByteString }
  deriving (Eq, Show)

-- | Smart constructor for the opaque @Token@ type.
-- @Token@ is a 1024-bit baseurl64-encoding-compliant nonce.
newToken :: IO Token
newToken = Token . mconcat <$> (sequence . replicate 8 $ Nonce.nonce128url nonceGen)

nonceGen :: Nonce.Generator
nonceGen = unsafePerformIO Nonce.new
{-# NOINLINE nonceGen #-}

instance ToJSON Token where
    toJSON = toJSON . BSC.unpack . unToken

-- * User

data User = User
    { username :: Text.Text
    , password :: Text.Text
    , email    :: Text.Text
    } deriving (Eq, Show, Generic, Read)

instance FromRow User where
    fromRow = User <$> field <*> field <*> field
