{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

{- This implementation is based on the description of the Recursive Ranking learning algorithm for OT layed out in § 7.3 of
- Kager, René. 2010. Optimality Theory. 10th printing. Cambridge Textbooks in Linguistics. Cambridge: Cambridge Univ. Press.
-}

module Learning where

import Data.List (findIndex, intercalate, minimumBy)
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import Data.Set (Set, delete, empty, fromList, insert, member, singleton, toAscList, union)
import Data.String (IsString (..))
import Data.Text (Text, pack, unpack)
import Debug.Trace

newtype Grammar = Grammar {strata :: [Set Constraint]}
  deriving (Eq)

instance Show Grammar where
  show grammar = intercalate " ≫ " $ map prettySet $ strata grammar
    where
      prettySet xs = "{" ++ intercalate ", " (map show (toAscList xs)) ++ "}"

c `rankIn` grammar = findIndex (\stratum -> c `member` stratum) (strata grammar)

dominates c1 c2 grammar =
  let i1 = c1 `rankIn` grammar
      i2 = c2 `rankIn` grammar
   in i1 < i2

newtype Constraint = Constraint {constraintName :: Text}
  deriving (Eq, Ord)

instance IsString Constraint where
  fromString = Constraint . pack

instance Show Constraint where
  show = unpack . constraintName

data MarkDataPair = MarkDataPair
  { loserMarks :: Set Constraint,
    winnerMarks :: Set Constraint
  }
  deriving (Show)

demoteWith :: MarkDataPair -> Grammar -> Grammar
demoteWith markDataPair grammar =
  trace ("\nexamining " ++ show markDataPair) $
    let highestRankedLoser = minimumBy (comparing (`rankIn` grammar)) (loserMarks markDataPair)
     in trace ("highest ranked loser: " ++ show highestRankedLoser) $
          foldr
            ( \winner g ->
                trace ("grammar: " ++ show g) $
                if (highestRankedLoser `dominates` winner) grammar
                  then g
                  else demoteBelow winner highestRankedLoser g
            )
            grammar
            (winnerMarks markDataPair)

demoteBelow :: Constraint -> Constraint -> Grammar -> Grammar
demoteBelow a b grammar =
  trace ("demoting " ++ show a ++ " below " ++ show b) $
    let rankB = fromJust $ b `rankIn` grammar
        strataWithoutA = map (delete a) (strata grammar)
     in grammar
          { strata =
              case splitAt rankB strataWithoutA of
                (before, bStratum : belowBStratum : after) -> before ++ bStratum : insert a belowBStratum : after
                (before, [bStratum]) -> before ++ [bStratum, singleton a]
          }

singleStratumFrom :: [MarkDataPair] -> Grammar
singleStratumFrom pairs = Grammar [foldr (\p g -> loserMarks p `union` winnerMarks p `union` g) empty pairs]

learnFrom :: [MarkDataPair] -> Grammar -> Grammar
learnFrom pairs g = go g (cycle pairs)
  where
    go g (p : ps) =
      let g' = demoteWith p g
       in if g' == g
            then g
            else go g' ps

hierarchyFrom :: [MarkDataPair] -> Grammar
hierarchyFrom = learnFrom <*> singleStratumFrom

markData :: [MarkDataPair]
markData =
  [ MarkDataPair {loserMarks = ["All-Ft-L"], winnerMarks = ["All-Ft-R"]},
    MarkDataPair {loserMarks = ["Parse-Syl"], winnerMarks = ["All-Ft-L", "All-Ft-R"]},
    MarkDataPair {loserMarks = ["All-Ft-L", "Leftmost", "Parse-Syl"], winnerMarks = ["All-Ft-R", "Rightmost"]},
    MarkDataPair {loserMarks = ["All-Ft-L", "Ft-Bin"], winnerMarks = ["Parse-Syl"]}
  ]

markData' :: [MarkDataPair]
markData' =
  [ MarkDataPair {loserMarks = ["Mid"], winnerMarks = ["Low"]},
    MarkDataPair {loserMarks = ["Top", "Low"], winnerMarks = ["Mid"]}
  ]
