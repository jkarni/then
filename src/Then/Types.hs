{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Then.Types where

import           Data.Aeson                         hiding (Success)
import qualified Data.Text                          as Text
import           GHC.Generics                       (Generic)
import           Database.PostgreSQL.Simple.FromRow (FromRow (..), field)

type Token = Text.Text

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

instance ToJSON Status where
    toJSON Success = String "success"
    toJSON Failure = String "error"


data LoginResult = LoginResult
    { loginResultStatus   :: Status
    , loginResultUserPath :: Text.Text
    , loginResultToken    :: Token
    } deriving (Eq, Show, Generic, Read)

instance ToJSON LoginResult where
    toJSON (LoginResult status path token) = object [ "status" .= status
                                                    , "user_path" .= path
                                                    , "user_token" .= token
                                                    ]


-- * User

data User = User
    { username :: Text.Text
    , password :: Text.Text
    , email    :: Text.Text
    } deriving (Eq, Show, Generic, Read)

instance FromRow User where
    fromRow = User <$> field <*> field <*> field
