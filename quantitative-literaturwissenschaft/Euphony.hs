module Euphony where

import Data.Set qualified as Set
import Debug.Trace
import Control.Monad

newtype Phoneme = MkPhoneme {getPhoneme :: String}
  deriving (Eq, Ord, Show)

vowels :: Set.Set Phoneme
vowels =
  Set.fromList $
    map
      MkPhoneme
      [ "a",
        "ɑ",
        "ɛ",
        "e",
        "ɪ",
        "i",
        "ɔ",
        "o",
        "ʊ",
        "u",
        "œ",
        "ø",
        "ʏ",
        "y",
        "ə"
      ]

consonants :: Set.Set Phoneme
consonants =
  Set.fromList $
    map
      MkPhoneme
      [ "m",
        "n",
        "ŋ",
        "p",
        "t",
        "k",
        "ʔ",
        "b",
        "d",
        "ɡ",
        "pf",
        "ts",
        "tʃ",
        "dʒ",
        "f",
        "s",
        "ʃ",
        "ç",
        "x",
        "h",
        "v",
        "z",
        "ʒ",
        "j",
        "l",
        "r"
      ]

choose :: Int -> Int -> Int
n `choose` k = product [1 .. n] `div` (product [1 .. k] * product [1 .. (n - k)])

binomialProbability :: Phoneme -> [Phoneme] -> Double
binomialProbability phoneme line =
  sum
    [ fromIntegral (setSize `choose` x)
        * p ** fromIntegral x
        * (1 - p) ** fromIntegral (setSize - x)
      | x <- [count .. setSize]
    ]
  where
    p :: Double
    p = join traceShow $ fromIntegral count / fromIntegral (Set.size set)
    count = length $ filter (== phoneme) line
    set
      | phoneme `Set.member` consonants = consonants
      | phoneme `Set.member` vowels = vowels
      | otherwise = error $ "Unknown phoneme: " ++ show phoneme
    setSize = length $ filter (`Set.member` set) line


test =
  let text = map MkPhoneme ["a", "d", "y", "n", "k", "a", "m", "a", "r", "e", "s", "u", "b", "a", "l", "u", "n", "e", "j", "f", "a", "ts", "ə"]
  in binomialProbability (MkPhoneme "a") text
