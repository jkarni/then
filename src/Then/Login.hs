{-# LANGUAGE OverloadedStrings #-}
module Then.Login where

import           Control.Monad                      (when)
import           Control.Monad.IO.Class             (liftIO)
import           Control.Monad.Trans.Either
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
import           Servant

import           Then.Types

loginByUsername :: ConnectInfo -> LoginByUsername -> EitherT ServantErr IO LoginResult
loginByUsername cinfo lbu = do
  let usr = loginByUsernameName lbu
  conn <- liftIO $ connect cinfo
  pwds <- liftIO $ query conn "SELECT password FROM users where username = ?" (Only usr)
  pwd  <- case pwds of
    [pwd] -> if pwd /= loginByUsernamePassword lbu then left err400 else return pwd
    _     -> left err400
  token <- liftIO newToken
  return $! LoginResult
    { loginResultStatus   = Success
    , loginResultUserPath = usr
    , loginResultToken    = token
    }

loginByEmail :: ConnectInfo -> LoginByEmail -> EitherT ServantErr IO LoginResult
loginByEmail = undefined

newToken :: IO Token
newToken = undefined