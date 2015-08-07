{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Then where

import qualified Data.ByteString.Char8      as BS
import           Data.Monoid
import           Data.Proxy
import           Database.PostgreSQL.Simple
import           Network.HTTP.Client        (Manager, defaultManagerSettings,
                                             newManager)
import           Network.HTTP.ReverseProxy  (ProxyDest (..))
import           Network.Wai.Handler.Warp   (run)
import           Servant

import           Then.Login
import           Then.Proxy
import           Then.Types


type API = "login_username" :> ReqBody '[JSON] LoginByUsername :> Post '[JSON] LoginResult
      :<|> "login_email"    :> ReqBody '[JSON] LoginByEmail    :> Post '[JSON] LoginResult
      :<|> Raw


api :: Proxy API
api = Proxy

proxyDest :: ProxyDest
proxyDest = undefined

server :: Manager -> Connection -> Server API
server mgr cinfo
  =    loginByUsername cinfo
  :<|> loginByEmail cinfo
  :<|> serviceProxy proxyDest mgr

main :: IO ()
main = do
    mgr <- newManager defaultManagerSettings
    conn <- connectPostgreSQL $ BS.pack $ "dbname=" <> dbName
    run 8080 . serve api $ server mgr conn

dbName :: String
dbName = "thendb"
