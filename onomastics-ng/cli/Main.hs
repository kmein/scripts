{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text.IO as Text
import Network.HTTP.Client.TLS (newTlsManager)
import Onomap.Stoepel
import Onomap.Svg
import Onomap.Types
import Options.Applicative

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
        let theName = Just $ surname options
        ds <- case areaMode options of
            State -> states
            District -> districts
        theStats <- case areaMode options of
            State -> stateStatistics theName
            District -> districtStatistics theName
        let stats = computeAreaStatistics computeFunction ds theStats
        return $ renderMap color ds stats
    Text.putStrLn res
