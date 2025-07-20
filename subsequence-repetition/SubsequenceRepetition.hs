module Main where

import Control.Monad (forM_)
import Data.Char (toLower)
import Data.List (sortOn)
import Data.Map qualified as Map
import Data.Set qualified as Set

slidingWindow :: Int -> [a] -> [[a]]
slidingWindow size list
  | size > 0 && length list >= size =
      let (window, rest) = splitAt size list
       in window : slidingWindow size (drop 1 list)
  | otherwise = []

findRepetitions :: (Ord a) => [[a]] -> Map.Map [a] (Set.Set Int)
findRepetitions windows =
  Map.filter ((> 1) . Set.size) $
    foldr
      (\(i, window) acc -> Map.insertWith Set.union window (Set.singleton i) acc)
      Map.empty
      (zip [0 ..] windows)

pairwiseComparison :: (Eq a) => [[a]] -> [([a], Int, Int)]
pairwiseComparison xs = [(x1, i, j) | (i, x1) <- ixs, (j, x2) <- ixs, x1 == x2, i < j]
  where
    ixs = zip [0 ..] xs

allPairwiseComparisons :: (Eq a) => [a] -> [[([a], Int, Int)]]
allPairwiseComparisons xs = map (\n -> pairwiseComparison (slidingWindow n xs)) [0 .. length xs]

allRepetitions :: (Ord a) => [a] -> [Map.Map [a] (Set.Set Int)]
allRepetitions xs = map (\n -> findRepetitions (slidingWindow n xs)) $ reverse [0 .. length xs]

printAllRepetitions :: String -> [Map.Map String (Set.Set Int)] -> IO ()
printAllRepetitions str substrRepetitions = do
  putStrLn str
  forM_ (sortOn score $ Map.toList $ Map.unions substrRepetitions) $ \(key, value) -> do
    putStr $
      zipWith
        (const $ \x -> if x then '*' else ' ')
        str
        [i `elem` concatMap (\x -> [x .. x + length key - 1]) (Set.toList value) | i <- [0 ..]]
    putStrLn $ "\t" ++ show key
  where
    score (substring, occurrences) = negate $ length substring ^ 2 * Set.size occurrences

printPairwiseComparison :: [(Int, Int)] -> IO ()
printPairwiseComparison [] = return ()
printPairwiseComparison xys =
  let (xs, ys) = unzip xys
      maxX = maximum xs
      maxY = maximum ys
      lastDigit = last . show
   in do
        putChar ' '
        forM_ [0 .. maxY] $ \y -> do
          putChar (lastDigit y)
        putChar '\n'
        forM_ [0 .. maxX] $ \x -> do
          putChar (lastDigit x)
          forM_ [0 .. maxY] $ \y -> do
            putChar $ if (x, y) `elem` xys then 'x' else '_'
          putChar '\n'
        putChar '\n'

main = do
  let str = map toLower $ (" " ++) $ "nachts im dichten fichtendickicht da sind dicke fichten wichtig"
  printAllRepetitions str (allRepetitions str)
