{-# LANGUAGE OverloadedStrings #-}
module Then.Types where

import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)
import Data.Aeson hiding (Success)

type Token = String


data LoginByUsername = LoginByUsername
    { loginByUsernameName     :: String
    , loginByUsernamePassword :: String
    } deriving (Eq, Show)

instance FromJSON LoginByUsername where
    parseJSON (Object v) = LoginByUsername <$> v .: "name"
                                           <*> v .: "password"

data LoginByEmail = LoginByEmail
    { loginByEmailName     :: String
    , loginByEmailPassword :: String
    } deriving (Eq, Show)

instance FromJSON LoginByEmail where
    parseJSON (Object v) = LoginByEmail <$> v .: "email"
                                        <*> v .: "password"


data LoginStatus = Success | Failure
    deriving (Eq, Show, Read)

instance ToJSON LoginStatus where
    toJSON Success = String "success"
    toJSON Failure = String "error"


data LoginResult = LoginResult
    { loginResultStatus   :: LoginStatus
    , loginResultUserPath :: String
    , loginResultToken    :: Token
    } deriving (Eq, Show, Read)

instance ToJSON LoginResult where
    toJSON (LoginResult status path token) = object [ "status" .= status
                                                    , "user_path" .= path
                                                    , "user_token" .= token
                                                    ]

data User = User
    { username :: String
    , password :: String
    , email    :: String
    } deriving (Eq, Show, Read)

instance FromRow User where
    fromRow = User <$> field <*> field <*> field
