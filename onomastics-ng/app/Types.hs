{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Types (Area (..), AreaKind (..), ByArea (..), relativeCount, absoluteCount, computeAreaStatistics) where

import Control.Arrow ((&&&))
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Foldable (toList)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Vector ((!))
import GHC.Generics

data AreaKind = District | State

newtype Key (t :: AreaKind) = Key {getKey :: Text}
    deriving (Show, Generic, Eq, Ord)

instance FromJSON (Key t) where
    parseJSON = withText "Area key" (pure . Key)

data Area (t :: AreaKind) = Area
    { name :: Text
    , key :: Key t
    , population :: Int
    , path :: Text
    }
    deriving (Show, Generic)

instance FromJSON (Area t)

newtype ByArea (t :: AreaKind) n = ByArea {getByArea :: Map (Key t) n}
    deriving (Show)

instance FromJSON (ByArea 'District Int) where
    parseJSON = parseClusters "DistrictClusterer"

instance FromJSON (ByArea 'State Int) where
    parseJSON = parseClusters "StateClusterer"

parseClusters :: Text -> Value -> Parser (ByArea t Int)
parseClusters clusterType =
    withObject "Statistics" $ \o ->
        ((o .: "clusterers") >>= (.: clusterType) >>= (.: "Data"))
            >>= withArray
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
                                                (\v -> return (Key k, truncate v))
                                                (kv ! 1)
                                        )
                                        (kv ! 0)
                                )
                            )
                            a
                )

computeAreaStatistics :: (Area t -> Int -> a) -> [Area t] -> ByArea t Int -> ByArea t a
computeAreaStatistics f areas nameCounts =
    ByArea . Map.fromList $ map (key &&& areaCount) areas
  where
    areaCount area =
        let nameCount = fromMaybe 0 (Map.lookup (key area) (getByArea nameCounts))
         in f area nameCount

absoluteCount, relativeCount :: Area t -> Int -> Double
absoluteCount _ count = fromIntegral count
relativeCount area count = million * (fromIntegral count / fromIntegral (population area))
  where
    million = 10 ** 6
