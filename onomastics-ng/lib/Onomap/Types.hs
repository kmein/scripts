{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Onomap.Types (Area (..), AreaKind (..), ByArea (..), relativeCount, absoluteCount, computeAreaStatistics, Mode(..)) where

import Control.Arrow ((&&&))
import Data.Aeson
import Control.Applicative (Alternative(..))
import Data.Foldable (toList)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Vector ((!))
import GHC.Generics
import Servant.API

data Mode = Relative | Absolute

instance FromHttpApiData Mode where
  parseUrlPiece = \case
    "relative" -> Right Relative
    "absolute" -> Right Absolute
    x -> Left x

data AreaKind = District | State

instance FromHttpApiData AreaKind where
  parseUrlPiece = \case
    "district" -> Right District
    "state" -> Right State
    x -> Left x

data Area = Area
    { name :: Text
    , key :: Text
    , population :: Int
    , path :: Text
    }
    deriving (Show, Generic)

instance FromJSON Area
instance ToJSON Area

newtype ByArea n = ByArea {getByArea :: Map Text n}
    deriving (Show)

instance ToJSON a => ToJSON (ByArea a) where
  toJSON = toJSON . getByArea

instance (Integral a, FromJSON a) => FromJSON (ByArea a) where
    parseJSON =
      withObject "Statistics" $ \o -> do
        clusterers <- o .: "clusterers"
        clusterer <- clusterers .: "DistrictClusterer" <|> clusterers .: "StateClusterer"
        data_ <- clusterer .: "Data"
        withArray
            "Clusters"
            ( \a ->
                ByArea . Map.fromList . toList
                    <$> mapM
                        ( withArray
                            "Cluster"
                            ( \kv ->
                                withText
                                    "Key"
                                    ( \k ->
                                        withScientific
                                            "Value"
                                            (\v -> return (k, truncate v))
                                            (kv ! 1)
                                    )
                                    (kv ! 0)
                            )
                        )
                        a
            )
            data_

computeAreaStatistics :: (Area -> Int -> a) -> [Area] -> ByArea Int -> ByArea a
computeAreaStatistics f areas nameCounts =
    ByArea . Map.fromList $ map (key &&& areaCount) areas
  where
    areaCount area =
        let nameCount = fromMaybe 0 (Map.lookup (key area) (getByArea nameCounts))
         in f area nameCount

absoluteCount, relativeCount :: Area -> Int -> Double
absoluteCount _ count = fromIntegral count
relativeCount area count = million * (fromIntegral count / fromIntegral (population area))
  where
    million = 10 ** 6
