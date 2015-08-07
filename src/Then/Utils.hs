{-# LANGUAGE OverloadedStrings #-}
module Then.Utils (loginError, errWithBody, newToken) where

import Data.Aeson (encode)
import Servant (ServantErr(..))
import Then.Types
import Data.Text ()
import System.IO.Unsafe (unsafePerformIO)

import qualified Crypto.Nonce as Nonce

-- * Errors

loginError :: Error
loginError = Error { location = "body"
                   , description = "User doesn't exist or password is wrong"
                   , name = ""
                   }


errWithBody :: ServantErr -> [Error] -> ServantErr
errWithBody se e = se { errBody = encode
    ErrorResponse { status = Failure, errors = e }}
