{-# LANGUAGE OverloadedStrings #-}
module Then.Proxy where

import Data.ByteString hiding (filter)
import Network.HTTP.Types ()
import Network.HTTP.ReverseProxy
import Network.HTTP.Client (Manager)
import Network.Wai


serviceProxy :: ProxyDest -> Manager -> Application
serviceProxy proxy = waiProxyTo reverseProxyHandler defaultOnExc
  where
    reverseProxyHandler req = do
      req' <- modifyReq req
      return $! WPRModifiedRequest req' proxy

-- | Modifies a request so that the X-User-Path can be trusted.
modifyReq :: Request -> IO Request
modifyReq req = case lookup "X-User-Path" $ requestHeaders req of
  Nothing -> return req
  Just p  -> let notAuthdReq = req{
          requestHeaders = filter (\(a,_) -> a /= "X-User-Path") $ requestHeaders req}
    in case lookup "X-User-Token" $ requestHeaders req of
      Nothing -> return notAuthdReq
      Just t  -> tokenMatches t p >>= \b -> return $! if b then req else notAuthdReq

-- | Lookup in DB whether a token exists and belongs to a user
tokenMatches :: ByteString -> ByteString -> IO Bool
tokenMatches token user = return $ token == "god-token" && user == "god"
