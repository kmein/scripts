{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Stoepel (districts, districtStatistics, states, stateStatistics, runStoepel) where

import Data.Proxy
import Data.Text (Text)
import Network.HTTP.Client (Manager)
import Servant.API
import Servant.Client
import Types (Area, AreaKind (..), ByArea)

type StoepelAPI =
    "content" :> "de" :> "districts.json" :> Get '[JSON] [Area 'District]
        :<|> "api" :> "clusters" :> "district" :> QueryParam "name" Text :> Get '[JSON] (ByArea 'District Int)
        :<|> "content" :> "de" :> "states.json" :> Get '[JSON] [Area 'State]
        :<|> "api" :> "clusters" :> "state" :> QueryParam "name" Text :> Get '[JSON] (ByArea 'State Int)

stoepelApi :: Proxy StoepelAPI
stoepelApi = Proxy

districts :: ClientM [Area 'District]
states :: ClientM [Area 'State]
districtStatistics :: Maybe Text -> ClientM (ByArea 'District Int)
stateStatistics :: Maybe Text -> ClientM (ByArea 'State Int)
districts :<|> districtStatistics :<|> states :<|> stateStatistics = client stoepelApi

runStoepel :: Manager -> ClientM a -> IO a
runStoepel manager' c = do
    x <- runClientM c (mkClientEnv manager' (BaseUrl Https "geogen.stoepel.net" 443 ""))
    case x of
        Left err -> error $ show err
        Right a -> return a
