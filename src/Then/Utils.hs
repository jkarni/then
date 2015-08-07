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
                   }

errWithBody :: ServantErr -> Error -> ServantErr
errWithBody se e = se { errBody = encode e }
