module Pissoir where

import Data.Tree
import Data.List
import Data.Function (on)
import qualified Data.Set as Set

type Urinal = Int

-- |Occupied ∪ Free| = n (size of pissoir)
-- Occupied ∩ Free = ∅
data Pissoir = Pissoir {occupied :: Set.Set Urinal, free :: Set.Set Urinal}
  deriving (Show)

maximumsBy :: (Ord o, Eq a) => (a -> o) -> [a] -> [a]
maximumsBy f xs = filter ((== theMaximum) . f) xs
  where theMaximum = maximum $ map f xs

mkPissoir :: Int -> Pissoir
mkPissoir number = Pissoir {occupied = Set.empty, free = Set.fromList [1..number]}

next :: Pissoir -> [Pissoir]
next pissoir
  | Set.null (occupied pissoir) = insertPossibilites (Set.toList (free pissoir))
  | Set.null (free pissoir) = []
  | otherwise =
      let distances = (\freeOne -> map (abs . subtract freeOne) $ Set.toList $ occupied pissoir)
          urinals = maximumsBy (minimum . distances) $ Set.toList $ free pissoir
      in insertPossibilites urinals
  where
    insertPossibilites =
      map
        (\urinal ->
          pissoir
          { occupied = Set.insert urinal (occupied pissoir)
          , free = Set.delete urinal (free pissoir)
          }
        )

renderPissoir :: Pissoir -> String
renderPissoir pissoir =
  map
    (\x ->
      if Set.member x (occupied pissoir)
        then '■'
        else if Set.member x (free pissoir)
          then '□'
          else '?')
    allUrinals
  where allUrinals = sort (Set.toList (Set.union (occupied pissoir) (free pissoir)))

pissoirTree :: Pissoir -> Tree Pissoir
pissoirTree initialPissoir = Node initialPissoir (map pissoirTree $ next initialPissoir)

printPissoirTree :: Tree Pissoir -> IO ()
printPissoirTree = putStrLn . drawTree . fmap renderPissoir

-- [1,2,4,8,20,48,216,576,1392,7200,43200,184320,
pissoirProblemN :: Int -> Int
pissoirProblemN = length . last . levels . pissoirTree . mkPissoir
