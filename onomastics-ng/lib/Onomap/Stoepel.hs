{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Onomap.Stoepel (districts, districtStatistics, states, stateStatistics, runStoepel) where

import Data.Proxy
import Data.Text (Text)
import Network.HTTP.Client (Manager)
import Servant.API
import Servant.Client
import Onomap.Types (Area, ByArea)

type StoepelAPI =
    "content" :> "de" :> "districts.json" :> Get '[JSON] [Area]
        :<|> "api" :> "clusters" :> "district" :> QueryParam "name" Text :> Get '[JSON] (ByArea Int)
        :<|> "content" :> "de" :> "states.json" :> Get '[JSON] [Area]
        :<|> "api" :> "clusters" :> "state" :> QueryParam "name" Text :> Get '[JSON] (ByArea Int)

stoepelApi :: Proxy StoepelAPI
stoepelApi = Proxy

districts :: ClientM [Area]
states :: ClientM [Area]
districtStatistics :: Maybe Text -> ClientM (ByArea Int)
stateStatistics :: Maybe Text -> ClientM (ByArea Int)
districts :<|> districtStatistics :<|> states :<|> stateStatistics = client stoepelApi

runStoepel :: Manager -> ClientM a -> IO a
runStoepel manager' c = do
    x <- runClientM c (mkClientEnv manager' (BaseUrl Https "geogen.stoepel.net" 443 ""))
    case x of
        Left err -> error $ show err
        Right a -> return a
