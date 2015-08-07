{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
module Then.Login where

import           Control.Monad                      (void)
import           Control.Monad.IO.Class             (liftIO)
import           Control.Monad.Trans.Either
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.SqlQQ   (sql)
import           Servant

import           Then.Types
import           Then.Utils

loginByUsername :: Connection -> LoginByUsername -> EitherT ServantErr IO LoginResult
loginByUsername conn lbu = do
  let usr = loginByUsernameName lbu
  pwds <- liftIO $ query conn [sql| SELECT password
                                    FROM account
                                    WHERE username = ? |] (Only usr)
  case pwds of
    [Only pwd] -> if pwd /= loginByUsernamePassword lbu
      then left $ err400 `errWithBody` [loginError]
      else return pwd
    _     -> left $ err400 `errWithBody` [loginError]
  token <- liftIO newToken
  return $! LoginResult
    { loginResultStatus   = Success
    , loginResultUserPath = usr
    , loginResultToken    = token
    }

loginByEmail :: Connection -> LoginByEmail -> EitherT ServantErr IO LoginResult
loginByEmail = undefined

createAccount :: Connection -> UserCreation -> EitherT ServantErr IO LoginResult
createAccount = undefined

-- | Should only be called after email validation or for testing
createUser :: Connection -> User -> IO ()
createUser conn User{..} = void $ execute conn [sql|
    INSERT INTO account (username, email, password)
    VALUES (?, ?, ?)
    |] (username, email, password)

removeUser :: Connection -> User -> IO ()
removeUser conn User{..} = void $ execute conn [sql|
    DELETE FROM account
    WHERE username = ?
    |] (Only username)
