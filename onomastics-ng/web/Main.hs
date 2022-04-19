{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad (liftM2)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON (..))
import Data.List (find)
import qualified Data.Map as Map (fromList, mapWithKey)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.Wai.Handler.Warp (runEnv)
import Onomap.Stoepel
import Onomap.Svg (renderMap)
import Onomap.Types
import Servant
import Servant.HTML.Blaze (HTML)
import Text.Blaze (ToMarkup (..))

data Response = Response {color :: Text, areas :: [Area], statistics :: ByArea Double}
    deriving (Generic)

instance ToMarkup Response where
    toMarkup response =
        preEscapedToMarkup $
            renderMap
                (color response)
                (areas response)
                (statistics response)

instance ToJSON Response where
    toJSON response =
        toJSON $
            Map.mapWithKey
                ( \k double ->
                    let maybeArea = find ((== k) . key) (areas response)
                     in Map.fromList
                            [ ("value" :: Text, toJSON double)
                            , ("population", toJSON $ fmap population maybeArea)
                            , ("name", toJSON $ fmap name maybeArea)
                            ]
                )
                (getByArea $ statistics response)

type OnomapApi =
    Capture "mode" Mode
        :> Capture "name" Text
        :> QueryParam "by" AreaKind
        :> QueryParam "color" Text
        :> Get '[JSON, HTML] Response

app :: Manager -> ([Area], [Area]) -> Application
app manager' (theDistricts, theStates) = serve onomapApi server
  where
    server :: Server OnomapApi
    server = \mode surname maybeAreaKind maybeColor ->
        liftIO $
            runStoepel manager' $ do
                let areaMode = fromMaybe District maybeAreaKind
                    computeFunction =
                        case mode of
                            Relative -> relativeCount
                            Absolute -> absoluteCount
                    theAreas = case areaMode of
                        State -> theStates
                        District -> theDistricts
                theStatistics <-
                    computeAreaStatistics computeFunction theAreas <$> case areaMode of
                        State -> stateStatistics (Just surname)
                        District -> districtStatistics (Just surname)
                return Response{color = fromMaybe "black" maybeColor, areas = theAreas, statistics = theStatistics}
    onomapApi :: Proxy OnomapApi
    onomapApi = Proxy

main :: IO ()
main = do
    manager' <- newTlsManager
    runEnv 8081 . app manager' =<< runStoepel manager' (liftM2 (,) districts states)
