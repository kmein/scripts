{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Network.HTTP.Client.TLS (newTlsManager)
import Options.Applicative
import Stoepel
import Svg
import Types

data Mode = Relative | Absolute

data Options = Options
    { mode :: Mode
    , surname :: Text
    , fillColor :: Maybe Text
    , areaMode :: AreaKind
    }

parseOptions :: Parser Options
parseOptions =
    Options
        <$> flag Absolute Relative (long "relative" <> help "Relative numbers (instead of absolute)")
        <*> strArgument (metavar "SURNAME" <> help "Surname")
        <*> optional (strOption (long "color" <> metavar "COLOR" <> help "Color of the SVG"))
        <*> flag District State (long "states" <> help "Analyze by state (instead of district)")

opts :: ParserInfo Options
opts = info (parseOptions <**> helper) (fullDesc <> progDesc "Map your German surname")

main :: IO ()
main = do
    options <- execParser opts
    manager' <- newTlsManager
    let computeFunction =
            case mode options of
                Relative -> relativeCount
                Absolute -> absoluteCount
        color = fromMaybe "black" $ fillColor options
    res <- runStoepel manager' $ do
        case areaMode options of
            State -> do
                ds <- states
                stats <- computeAreaStatistics computeFunction ds <$> stateStatistics (Just $ surname options)
                return $ drawMap color ds stats
            District -> do
                ds <- districts
                stats <- computeAreaStatistics computeFunction ds <$> districtStatistics (Just $ surname options)
                return $ drawMap color ds stats
    print res
