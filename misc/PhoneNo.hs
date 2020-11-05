{-# LANGUAGE LambdaCase #-}
import Data.Char

letters = \case
  2 -> "ABC"
  3 -> "DEF"
  4 -> "GHI"
  5 -> "JKL"
  6 -> "MNO"
  7 -> "PQRS"
  8 -> "TUV"
  9 -> "WXYZ"
  _ -> []

main :: IO ()
main = interact $ unlines . possibleWords . digitsOf
  where
    possibleWords = mapM letters
    digitsOf = map digitToInt . filter isDigit
