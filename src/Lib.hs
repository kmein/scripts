{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
module Lib
  ( someFunc
  )
where
import           Control.Monad                  ( forM_ )
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


data Manner = Tenuis | Media | MediaAspirata
  deriving (Show, Eq)

data Place = Labial | Dental | Palatal | Velar | Labiovelar
  deriving (Show, Eq)

data Phoneme
  = E | O | A | E_ | O_ | A_
  | I | U | N | M | R | L
  | P | B | Bh | T | D | Dh | Kj | Gj | Gjh | K | G | Gh | Kw | Gw | Gwh | S | H1 | H2 | H3 | H
  deriving (Eq, Ord)

instance Show Phoneme where
  show = \case
    E   -> "e"
    O   -> "o"
    A   -> "a"
    E_  -> "ē"
    O_  -> "ō"
    A_  -> "ā"
    I   -> "i"
    U   -> "u"
    N   -> "n"
    M   -> "m"
    R   -> "r"
    L   -> "l"
    P   -> "p"
    B   -> "b"
    Bh  -> "bh"
    T   -> "t"
    D   -> "d"
    Dh  -> "dh"
    Kj  -> "ḱ"
    Gj  -> "ǵ"
    Gjh -> "ǵh"
    K   -> "k"
    G   -> "g"
    Gh  -> "gh"
    Kw  -> "kʷ"
    Gw  -> "gʷ"
    Gwh -> "gʷh"
    S   -> "s"
    H1  -> "h₁"
    H2  -> "h₂"
    H3  -> "h₃"
    H   -> "H"

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
  | phoneme `elem` [H1, H2, H3, H] = Laryngeal
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


syllabify :: [Phoneme] -> [Bool]
syllabify = foldr isSonant []
 where
  hasToBeSyllabic p = features p == Vowel
  canBeSyllabic p = features p `elem` [Vowel, Resonant, Laryngeal]
  isSonant current xs = case xs of
    []                   -> [hasToBeSyllabic current] -- last phoneme in word
    previousIsSonant : _ -> if previousIsSonant
      then hasToBeSyllabic current : xs -- only vowels are sonant, the rest can be consonantal
      else canBeSyllabic current : xs -- make syllabic if previous is not syllabic

showSyllabic :: Phoneme -> Bool -> String
showSyllabic phoneme syllabic = case (phoneme, syllabic) of
  (I , False) -> "y"
  (U , False) -> "w"
  (N , True ) -> "ṇ"
  (M , True ) -> "ṃ"
  (R , True ) -> "ṛ"
  (L , True ) -> "ḷ"
  (H1, True ) -> "ə₁"
  (H2, True ) -> "ə₂"
  (H3, True ) -> "ə₃"
  (H , True ) -> "ə"
  _           -> show phoneme

printSyllabified ps =
  putStr (concatMap show ps) >> putChar ' ' >> print (syllabify ps)

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
        ]
  forM_ pieLines
    $ \line -> putStrLn $ concat $ zipWith showSyllabic line (syllabify line)
