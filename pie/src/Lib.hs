{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Lib
  ( someFunc
  )
where

import           Phonology
import           Util                           ( replaceSublist )

import           Control.Monad                  ( forM_ )
import           Data.List                      ( zip3 )

{-
import           Data.Void
import qualified Text.Megaparsec               as Parsec
import qualified Text.Megaparsec.Stream        as Parsec

instance Parsec.Stream [Phoneme] where
  type Token [Phoneme] = Phoneme
  type Tokens [Phoneme] = [Phoneme]
  tokensToChunk _ = id
  chunkToTokens _ = id
  chunkLength _ = length
  take1_ []       = Nothing
  take1_ (x : xs) = Just (x, xs)
  takeN_ n xs = if length xs >= n then Just (splitAt n xs) else Nothing
  takeWhile_ = span
  showTokens _ = show
  reachOffset offset posState =
    Parsec.reachOffset' splitAt foldl' id id ('\n', '\t') o pst



parseRoot :: Parsec.Parsec Void [Phoneme] ()
parseRoot = () <$ do
  Parsec.optional $ satisfyFeature (`elem` [Sibilant, Laryngeal])
  Parsec.optional $ satisfyFeature isPlosive
  satisfyFeature $ \p -> isPlosive p || p == Resonant
  Parsec.single E
  satisfyFeature $ \p -> isPlosive p || p == Resonant
  Parsec.optional $ satisfyFeature isPlosive
  Parsec.optional $ satisfyFeature (`elem` [Sibilant, Laryngeal])
 where
  satisfyFeature :: (FeatureSet -> Bool) -> Parsec.Parsec Void [Phoneme] Phoneme
  satisfyFeature p = Parsec.satisfy (p . features)
-}



syllabify :: [Phoneme] -> [Bool]
syllabify = replaceSublist [True, True] [True, False]  -- replace VV by VC
                                                      . foldr isSonant []
 where
  hasToBeSyllabic p = features p == Vowel
  canBeSyllabic p = features p `elem` [Vowel, Resonant]
  isSonant current xs = case xs of
    []                   -> [canBeSyllabic current] -- make last phoneme in word syllabic
    previousIsSonant : _ -> if previousIsSonant
      then hasToBeSyllabic current : xs -- only vowels are sonant, the rest can be consonantal
      else canBeSyllabic current : xs -- make syllabic if previous is not syllabic


syllableCores :: [Phoneme] -> [Bool]
syllableCores = localMax . (++ [0]) . ([0] ++) . map (sonority . features)
 where
  localMax xs = map project $ zip3 xs (tail xs) (drop 2 xs)
    where project (x, y, z) = x <= y && y >= z

-- chunkSyllables :: [Bool] -> [[Bool]]
-- chunkSyllables = _

someFunc :: IO ()
someFunc = do
  let pieLines =
        [ [Kj, M, T, O, M]
        , [N, E, H2, U, S]
        , [I, U, N, E, G, T, I]
        , [I, U, N, G, E, N, T, I]
        , [H2, N, R, Bh, I]
        , [H2, E, R, H3, T, R, O, M]
        , [H2, I, U, H, O, N]
        , [H2, S, N, T, I, E, H2]
        , [Gh, E_, I, M, N, O]
        , [Kj, U, N, Bh, I]
        , [D, I, U, O, S]
        , [N, M, R, T, O, S]
        , [U, L, Kw, O, S]
        , [Kj, U, N, O, S]
        , [D, N, T, N, S]
        , [N, D, R, Kj, T, O, S]
        , [U, N, T, O, S]
        , [D, U, I, D, Kj, M, T]
        , [Kj, L, U, Dh, I]
        , [H1, N, E, U, N]
        , [H2, U, E, H1, N, T, O, S]
        ]
  forM_ pieLines $ \line -> do
    putStr $ concatMap show line
    putChar ' '
    putStrLn $ concat $ zipWith showSyllabic line (syllabify line)
