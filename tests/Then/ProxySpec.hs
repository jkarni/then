{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Then.ProxySpec (spec) where

import           Control.Arrow
import           Control.Concurrent
import           Control.Lens              hiding (elements)
import           Control.Monad             (join)
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.CaseInsensitive      (mk)
import           Data.Char                 (isAlphaNum)
import           Data.List
import           Network.HTTP.Client       (defaultManagerSettings, newManager)
import           Network.HTTP.ReverseProxy (ProxyDest (..))
import           Network.HTTP.Types        hiding (statusCode)
import           Network.Wai               hiding (responseStatus)
import           Network.Wai.Handler.Warp  (run)
import           Network.Wreq              hiding (proxy)
import           Test.Hspec
import           Test.QuickCheck

import qualified Data.ByteString.Char8     as BS
import qualified Data.Text                 as Text

import           Then.Arbitrary
import           Then.Proxy


spec :: Spec
spec = do
  serviceProxySpec

serviceProxySpec :: Spec
serviceProxySpec = beforeAll setup $ afterAll teardown $ describe "serviceProxy" $ do

  it "proxies requests" $ const $ do
    property $ \rPath -> hitsProxy rPath ==> do
      resp <- get $ url 8082 rPath
      resp ^. responseStatus . statusCode `shouldBe` 299

  context "when there is no 'X-User-Path' header" $ do
    let noXUser = filter (\(h,_) -> h /= "X-User-Path")
    let cleanHeader = join (***) $ filter (\x -> isAlphaNum x || x == '-')

    it "proxies request bodies unmodified" $ const $ do
      property $ \(NonEmpty rBody :: NonEmptyList Char) rPath -> hitsProxy rPath ==> do
        resp <- post (url 8082 rPath) (BS.pack rBody)
        resp ^? responseBody . key "body" `shouldBe` Just (String $ Text.pack rBody)

    it "proxies headers unmodified" $ const $ do
      property $ \(NonEmpty rBody :: NonEmptyList Char) rHdrs rPath ->
        hitsProxy rPath ==> do
          let hdrs = (mk . BS.pack *** BS.pack) . cleanHeader <$> noXUser rHdrs
          let opts = defaults & headers .~ hdrs
          resp <- postWith opts (url 8082 rPath) (BS.pack rBody)
          let Just hdrs' = resp ^? responseBody . key "reqHeaders" . _JSON
          hdrs' `shouldContain` hdrs

-- * Utils

url :: Int -> String -> String
url port rPath = "http://localhost:" ++ show port ++ "/"
               ++ BS.unpack (urlEncode True $ BS.pack rPath)


setup :: IO (ThreadId, ThreadId)
setup = do
    mgr <- newManager defaultManagerSettings
    let proxy = ProxyDest "127.0.0.1" 8081
    proxyThread <- forkIO $ run 8081 proxyDestServer
    appThread   <- forkIO $ run 8082 $ serviceProxy proxy mgr
    return (proxyThread, appThread)

teardown :: (ThreadId, ThreadId) -> IO ()
teardown (proxyThread, appThread) = killThread proxyThread >> killThread appThread

proxyDestServer :: Application
proxyDestServer request respond = do
    bd <- requestBody request
    let reqInfo = RequestInfo { path = rawPathInfo request
                              , reqHeaders = requestHeaders request
                              , method = show $ requestMethod request
                              , body = bd
                              }
    respond $ responseLBS (mkStatus 299 "") [] (encode reqInfo)

hitsProxy :: String -> Bool
hitsProxy = not . or . sequence [ ("principals/users" `isPrefixOf`)
                                , ("activate_account" `isPrefixOf`)
                                , ("login_username" `isPrefixOf`)
                                , ("login_email" `isPrefixOf`)
                                ]
