{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeFamilies #-}
module Lib
  ( someFunc
  )
where

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



data Manner = Tenuis | Media | MediaAspirata
  deriving (Show, Eq)

data Place = Labial | Dental | Palatal | Velar | Labiovelar
  deriving (Show, Eq)

data Phoneme
  = E | O | A | E_ | O_ | A_
  | I | U | N | M | R | L
  | P | B | Bh | T | D | Dh | Kj | Gj | Gjh | K | G | Gh | Kw | Gw | Gwh | S | H1 | H2 | H3
  deriving (Eq, Ord)

data FeatureSet
  = Vowel
  | Resonant
  | Plosive { manner :: Manner, place :: Place }
  | Sibilant
  | Laryngeal
  deriving (Show, Eq)

isPlosive :: FeatureSet -> Bool
isPlosive (Plosive _ _) = True
isPlosive _             = False

features :: Phoneme -> FeatureSet
features phoneme
  | phoneme == S = Sibilant
  | phoneme `elem` [E, O, A, E_, O_, A_] = Vowel
  | phoneme `elem` [H1, H2, H3] = Laryngeal
  | phoneme `elem` [I, U, N, M, R, L] = Resonant
  | otherwise = Plosive
    { manner = if
                 | phoneme `elem` [P, T, K, Kj, Kw]      -> Tenuis
                 | phoneme `elem` [B, D, G, Gj, Gw]      -> Media
                 | phoneme `elem` [Bh, Dh, Gh, Gjh, Gwh] -> MediaAspirata
    , place  = if
                 | phoneme `elem` [P, B, Bh]    -> Labial
                 | phoneme `elem` [T, D, Dh]    -> Dental
                 | phoneme `elem` [Kj, Gj, Gjh] -> Palatal
                 | phoneme `elem` [K, G, Gh]    -> Velar
                 | phoneme `elem` [Kw, Gw, Gwh] -> Labiovelar
    }



someFunc :: IO ()
someFunc = Parsec.parseTest parseRoot [P, R, E, Kj]
