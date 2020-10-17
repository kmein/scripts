{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
module Phonology where


data Manner = MediaAspirata | Media | Tenuis
  deriving (Show, Eq, Ord)

data Place = Labial | Dental | Palatal | Velar | Labiovelar
  deriving (Show, Eq, Ord)

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
  | Laryngeal
  | Sibilant
  | Plosive { manner :: Manner, place :: Place }
  deriving (Show, Eq, Ord)

sonority :: FeatureSet -> Int
sonority = \case
  Vowel       -> 4
  Resonant    -> 3
  Laryngeal   -> 3
  Sibilant    -> 2
  Plosive _ _ -> 1

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

showSyllabic :: Phoneme -> Bool -> String
showSyllabic phoneme syllabic = case (phoneme, syllabic) of
  (I , False) -> "y"
  (U , False) -> "w"
  (N , True ) -> "ṇ"
  (M , True ) -> "ṃ"
  (R , True ) -> "ṛ"
  (L , True ) -> "ḷ"
  -- (H1, True ) -> "ə₁"
  -- (H2, True ) -> "ə₂"
  -- (H3, True ) -> "ə₃"
  -- (H , True ) -> "ə"
  _           -> show phoneme

