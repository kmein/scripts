{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Text (Text)
import qualified Data.Text.IO as Text
import Network.HTTP.Client.TLS (newTlsManager)
import Onomap.Stoepel
import Onomap.Svg
import Onomap.Types
import Options.Applicative

data Options = Options
    { mode :: Mode
    , surnames :: [Text]
    , colorPalette :: [Text]
    , areaMode :: AreaKind
    }

parseOptions :: Parser Options
parseOptions =
    Options
        <$> flag Absolute Relative (long "relative" <> help "Relative numbers (instead of absolute)")
        <*> some (strArgument (metavar "SURNAME" <> help "Surname"))
        <*> many (strOption (long "color" <> metavar "COLOR" <> help "Color palette for the SVG"))
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
        colors = colorPalette options ++ defaultColorPalette
        svgSettings = SvgSettings{scaleToMaximum = Global}
    res <- runStoepel manager' $ do
        let theNames = map Just (surnames options)
        ds <- case areaMode options of
            State -> states
            District -> districts
        theStats <- case areaMode options of
            State -> mapM stateStatistics theNames
            District -> mapM districtStatistics theNames
        let stats = map (computeAreaStatistics computeFunction ds) theStats
        return $ renderMap svgSettings colors ds (zip (surnames options) stats)
    Text.putStrLn res
