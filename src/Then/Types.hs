{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Then.Types where

import           Data.Aeson                         hiding (Success)
import qualified Data.Text                          as Text
import           GHC.Generics                       (Generic)
import           Database.PostgreSQL.Simple.FromRow (FromRow (..), field)

type Token = String

-- * Errors

data Error = Error { location    :: Text.Text
                   , description :: Text.Text
                   } deriving (Eq, Show, Read, Generic, FromJSON, ToJSON)


-- * Login

data LoginByUsername = LoginByUsername
    { loginByUsernameName     :: String
    , loginByUsernamePassword :: String
    } deriving (Eq, Generic, Show)

instance FromJSON LoginByUsername where
    parseJSON (Object v) = LoginByUsername <$> v .: "name"
                                           <*> v .: "password"

data LoginByEmail = LoginByEmail
    { loginByEmailName     :: String
    , loginByEmailPassword :: String
    } deriving (Eq, Generic, Show)

instance FromJSON LoginByEmail where
    parseJSON (Object v) = LoginByEmail <$> v .: "email"
                                        <*> v .: "password"


data LoginStatus = Success | Failure
    deriving (Eq, Show, Generic, Read)

instance ToJSON LoginStatus where
    toJSON Success = String "success"
    toJSON Failure = String "error"


data LoginResult = LoginResult
    { loginResultStatus   :: LoginStatus
    , loginResultUserPath :: String
    , loginResultToken    :: Token
    } deriving (Eq, Show, Generic, Read)

instance ToJSON LoginResult where
    toJSON (LoginResult status path token) = object [ "status" .= status
                                                    , "user_path" .= path
                                                    , "user_token" .= token
                                                    ]


-- * User

data User = User
    { username :: String
    , password :: String
    , email    :: String
    } deriving (Eq, Show, Generic, Read)

instance FromRow User where
    fromRow = User <$> field <*> field <*> field
