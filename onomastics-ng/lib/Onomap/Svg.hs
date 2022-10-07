{-# LANGUAGE OverloadedStrings #-}

module Onomap.Svg (drawMap, renderMap) where

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Lazy (toStrict)
import Graphics.Svg
import Onomap.Types (Area (..), ByArea (..), ScaleToMaximum (..), SvgSettings (..))
import Text.Printf (printf)

renderMap :: SvgSettings -> [Text] -> [Area] -> [(Text, ByArea Double)] -> Text
renderMap settings colorPalette areas statistics = toStrict $ prettyText $ drawMap settings colorPalette areas statistics

drawMap :: SvgSettings -> [Text] -> [Area] -> [(Text, ByArea Double)] -> Element
drawMap settings colorPalette areas statistics =
    doctype <> with (svg11_ content) [Version_ <<- "1.1", Width_ <<- "650", Height_ <<- Text.pack (show (900 + 20 * length statistics))]
  where
    theMaxima = map maximum $ map (getByArea . snd) statistics
    globalMaximum = maximum theMaxima
    localize = Text.replace "." "," . Text.dropWhileEnd (== '.') . Text.dropWhileEnd (== '0')
    showRounded = Text.pack . printf "%.2f"
    frameColor = "black"
    areaPaths =
        foldMap
            ( \area ->
                path_ [Stroke_ <<- frameColor, Stroke_width_ <<- "0.3px", Fill_ <<- "none", D_ <<- path area]
                    <> foldMap
                        ( \((surname, statistic), color, theMaximum) ->
                            let count = fromMaybe 0 (Map.lookup (key area) (getByArea statistic))
                                theTitle =
                                    ( title_
                                        []
                                        ( toElement $
                                            mconcat
                                                [ name area
                                                , ": "
                                                , Text.intercalate
                                                    ", "
                                                    (map (\(surname, statistic) -> localize (showRounded count) <> " (" <> Text.toTitle surname <> ")") statistics)
                                                ]
                                        )
                                    )
                             in path_
                                    [ D_ <<- path area
                                    , Fill_ <<- color
                                    , Fill_opacity_
                                        <<- showRounded
                                            ( if count == 0
                                                then 0
                                                else
                                                    count
                                                        / fromIntegral (length statistics)
                                                        / ( case scaleToMaximum settings of
                                                                Global -> globalMaximum
                                                                Local -> theMaximum
                                                          )
                                            )
                                    , Stroke_ <<- "none"
                                    ]
                                    theTitle
                        )
                        (zip3 statistics (cycle colorPalette) theMaxima)
            )
            areas
    content =
        foldMap
            ( \(index, color, theMaximum) ->
                defs_
                    []
                    ( linearGradient_
                        [ Id_ <<- "legend" <> Text.pack (show index)
                        , X1_ <<- "0"
                        , X2_ <<- case scaleToMaximum settings of
                            Global -> Text.pack (show (recip $ theMaximum / globalMaximum))
                            Local -> "1"
                        , Y1_ <<- "0"
                        , Y2_ <<- "0"
                        ]
                        ( stop_ [Offset_ <<- "0%", Stop_color_ <<- "white"]
                            <> stop_ [Offset_ <<- "100%", Stop_color_ <<- color]
                        )
                    )
            )
            (zip3 [0 ..] colorPalette theMaxima)
            <> foldMap
                ( \(index, (name, statistic), color) ->
                    g_
                        []
                        ( rect_
                            [ X_ <<- "165"
                            , Y_ <<- Text.pack (show (880 + 20 * index))
                            , Stroke_ <<- frameColor
                            , Width_ <<- "350"
                            , Height_ <<- "10"
                            , Fill_ <<- "url(#legend" <> Text.pack (show index) <> ")"
                            ]
                            <> text_
                                [ X_ <<- "155"
                                , Y_ <<- Text.pack (show (890 + 20 * index))
                                , Font_size_ <<- "18"
                                , Fill_ <<- "black"
                                , Text_anchor_ <<- "end"
                                ]
                                (toElement $ Text.toTitle name)
                            <> text_
                                [ X_ <<- "525"
                                , Y_ <<- Text.pack (show (890 + 20 * index))
                                , Font_size_ <<- "18"
                                , Fill_ <<- "black"
                                ]
                                (toElement $ localize $ showRounded $ maximum $ getByArea statistic)
                        )
                )
                (zip3 [0 ..] statistics colorPalette)
            <> style_ [] "path { mix-blend-mode: multiply; }"
            <> g_ [] areaPaths
