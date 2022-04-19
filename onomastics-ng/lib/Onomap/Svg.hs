{-# LANGUAGE OverloadedStrings #-}

module Onomap.Svg (drawMap, renderMap) where

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import qualified Data.Text as Text
import Graphics.Svg
import Text.Printf (printf)
import Onomap.Types (Area (..), ByArea (..))

renderMap :: Text -> [Area] -> ByArea Double -> Text
renderMap fillColor areas statistics = toStrict $ prettyText $ drawMap fillColor areas statistics

drawMap :: Text -> [Area] -> ByArea Double -> Element
drawMap fillColor areas statistics =
    doctype <> with (svg11_ content) [Version_ <<- "1.1", Width_ <<- "650", Height_ <<- "900"]
  where
    theMaximum = maximum $ getByArea statistics
    localize = Text.replace "." "," . Text.dropWhileEnd (== '.') . Text.dropWhileEnd (== '0')
    showRounded = Text.pack . printf "%.2f"
    frameColor = "grey"
    areaPaths =
        foldMap
            ( \area ->
                let count = fromMaybe 0 (Map.lookup (key area) (getByArea statistics))
                 in path_
                        [ Stroke_ <<- frameColor
                        , Fill_ <<- fillColor
                        , Fill_opacity_ <<- showRounded (if count == 0 then 0 else count / theMaximum)
                        , D_ <<- path area
                        ]
                        ( title_
                            []
                            ( toElement $
                                name area <> ": " <> localize (showRounded count)
                            )
                        )
            )
            areas
    content =
        defs_
            []
            ( linearGradient_
                [ Id_ <<- "legend"
                , X1_ <<- "0"
                , X2_ <<- "1"
                , Y1_ <<- "0"
                , Y2_ <<- "0"
                ]
                ( stop_ [Offset_ <<- "0%", Stop_color_ <<- "white"]
                    <> stop_ [Offset_ <<- "100%", Stop_color_ <<- fillColor]
                )
            )
            <> g_
                []
                ( rect_
                    [ X_ <<- "150"
                    , Y_ <<- "880"
                    , Stroke_ <<- frameColor
                    , Width_ <<- "350"
                    , Height_ <<- "10"
                    , Fill_ <<- "url(#legend)"
                    ]
                    <> text_
                        [ X_ <<- "135"
                        , Y_ <<- "890"
                        , Font_size_ <<- "18"
                        , Fill_ <<- "black"
                        ]
                        "0"
                    <> text_
                        [ X_ <<- "510"
                        , Y_ <<- "890"
                        , Font_size_ <<- "18"
                        , Fill_ <<- "black"
                        ]
                        (toElement $ localize $ showRounded theMaximum)
                )
            <> g_ [] areaPaths
