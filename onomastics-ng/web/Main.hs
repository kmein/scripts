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
import qualified Data.Map as Map (fromList, lookup)
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

data Response = Response {color :: [Text], areas :: [Area], statistics :: [(Text, ByArea Double)]}
    deriving (Generic)

instance ToMarkup Response where
    toMarkup response =
        preEscapedToMarkup $
            renderMap
                (SvgSettings{scaleToMaximum = Global})
                (color response)
                (areas response)
                (statistics response)

instance ToJSON Response where
    toJSON response =
        toJSON $
            Map.fromList $
                map
                    ( \area ->
                        ( name area
                        , Map.fromList $
                            ("population", toJSON $ population area) :
                            map
                                ( \(surname, stats) ->
                                    ( surname
                                    , toJSON $ Map.lookup (key area) (getByArea stats)
                                    )
                                )
                                (statistics response)
                        )
                    )
                    (areas response)

type OnomapApi =
    Capture "mode" Mode
        :> QueryParams "name" Text
        :> QueryParam "by" AreaKind
        :> QueryParams "color" Text
        :> Get '[HTML] Response

app :: Manager -> ([Area], [Area]) -> Application
app manager' (theDistricts, theStates) = serve onomapApi server
  where
    server :: Server OnomapApi
    server = \mode (surnames :: [Text]) maybeAreaKind colors ->
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
                    theNames = map Just surnames
                theStats <- case areaMode of
                    State -> mapM stateStatistics theNames
                    District -> mapM districtStatistics theNames
                let stats = map (computeAreaStatistics computeFunction theAreas) theStats
                return
                    Response
                        { color = colors ++ defaultColorPalette
                        , areas = theAreas
                        , statistics = zip surnames stats
                        }
    onomapApi :: Proxy OnomapApi
    onomapApi = Proxy

main :: IO ()
main = do
    manager' <- newTlsManager
    runEnv 8081 . app manager' =<< runStoepel manager' (liftM2 (,) districts states)
