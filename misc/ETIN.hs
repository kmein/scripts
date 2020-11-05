{-# LANGUAGE OverloadedStrings #-}
import Data.Text (Text)
import qualified Data.Text as Text

isConsonant :: Char -> Bool
isConsonant = flip elem ("BCDFGHJKLMNPQRSTVWXYZ" :: String)

isVowel :: Char -> Bool
isVowel = flip elem ("AEIOU" :: String)

serialize :: Text -> Text
serialize =
  Text.filter (\c -> isConsonant c || isVowel c) .
  Text.replace "Ä" "AE" .
  Text.replace "Ö" "OE" .
  Text.replace "Ü" "UE" .
  Text.replace "ß" "SS" . Text.replace "SCH" "Y" . Text.toUpper

data Person =
  Person
  { lastName :: Text
  , firstName :: Text
  , dateOfBirth :: (Int, Int, Int)
  }

serializeName :: Text -> Text
serializeName n =
  Text.pack $
  take 4 $ filter isConsonant n' <> reverse (filter isVowel n') <> repeat 'X'
  where
    n' = Text.unpack $ serialize n

etin :: Person -> Text
etin (Person last first (y, m, d)) =
  let etin' =
        serializeName last <> serializeName first <> Text.pack (last2 (show y)) <>
        Text.singleton (encode (m - 1)) <>
        Text.pack (fill0 (show d))
   in etin' <> Text.singleton (encode $ checksum $ Text.unpack etin')
  where
    last2 = until (\x -> length x == 2) tail
    fill0 = until (\x -> length x == 2) ('0' :)
    encode = (!!) ['A' .. 'Z']
    checksum x = sum (zipWith value x (cycle [False, True])) `mod` 26

value :: Char -> Bool -> Int
value x = case x of
  '0' -> eo 0 1
  '1' -> eo 1 0
  '2' -> eo 2 5
  '3' -> eo 3 7
  '4' -> eo 4 9
  '5' -> eo 5 13
  '6' -> eo 6 15
  '7' -> eo 7 17
  '8' -> eo 8 19
  '9' -> eo 9 21
  'A' -> eo 0 1
  'B' -> eo 1 0
  'C' -> eo 2 5
  'D' -> eo 3 7
  'E' -> eo 4 9
  'F' -> eo 5 13
  'G' -> eo 6 15
  'H' -> eo 7 17
  'I' -> eo 8 19
  'J' -> eo 9 21
  'K' -> eo 10 2
  'L' -> eo 11 4
  'M' -> eo 12 18
  'N' -> eo 13 20
  'O' -> eo 14 11
  'P' -> eo 15 3
  'Q' -> eo 16 6
  'R' -> eo 17 8
  'S' -> eo 18 12
  'T' -> eo 19 14
  'U' -> eo 20 16
  'V' -> eo 21 10
  'W' -> eo 22 22
  'X' -> eo 23 23
  'Y' -> eo 24 24
  'Z' -> eo 25 25
  where eo e o b = if b then e else o
