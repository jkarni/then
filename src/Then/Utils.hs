{-# LANGUAGE OverloadedStrings #-}
module Then.Utils where

import Data.Aeson (encode)
import Servant (ServantErr(..))
import Then.Types
import Data.Text ()


-- * Errors

loginError :: Error
loginError = Error { location = "body"
                   , description = "User doesn't exist or password is wrong"
                   , name = "password"
                   }

emailTakenError :: Error
emailTakenError = Error { location = "body"
                        , description = "The user login email is not unique"
                        , name = "data.adhocracy_core.sheets.principal.IUserExtended.email"
                        }

usernameTakenError :: Error
usernameTakenError = undefined


errWithBody :: ServantErr -> [Error] -> ServantErr
errWithBody se e = se { errBody = encode
    ErrorResponse { status = Failure, errors = e }}
