module Main where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (replicateM)
import Data.Either
import Data.Random.Normal (normalIO)
import Debug.Trace
import Text.Printf

type Card = [Int]

data Side = A | B
  deriving (Show, Eq)

type Statistics = ((Float, Float), Float, Int)

sides = [A, B]

data State = State { turn :: Int, aCards :: [Card], bCards :: [Card], lastWinner :: Side, temp :: [Card] }
  deriving (Show)

data Parameters = Parameters { beginningSide :: Side, fields :: Int, cardsPerSide :: Int }
  deriving (Show)

maxIndex xs = head $ filter ((== maximum xs) . (xs !!)) [0..]

point :: IO Int
point = abs . truncate . (100 *) <$> (normalIO :: IO Float)

initialState :: Parameters -> IO State
initialState parameters =
  State 0 <$> cards <*> cards <*> pure (beginningSide parameters) <*> pure []
  where card = replicateM (fields parameters) point
        cards = replicateM (cardsPerSide parameters) card

runGame :: State -> Either State (Side, Int)
runGame state =
  if turn state >= 1000 then Left state  else
  case (aCards state, bCards state) of
    ([], _) -> Right (A, turn state)
    (_, []) -> Right (B, turn state)
    (a:ac, b:bc) ->
      let
        lastWinnersFirstCard = case lastWinner state of
          A -> a
          B -> b
        -- the last winning player picks the field of his card which is best
        bestField = maxIndex lastWinnersFirstCard
      in
        -- trace ("maxIndex of " ++ show lastWinnersFirstCard ++ " is " ++ show bestField) $
        -- trace ("Side " ++ show (lastWinner state) ++ " asking for " ++ show bestField) $
        runGame $ case compare (a !! bestField) (b !! bestField) of
          GT ->
            -- trace "A won" $
            state
              { turn = succ $ turn state
              , aCards = ac ++ temp state ++ [a, b]
              , bCards = bc
              , lastWinner = A
              , temp = []
              }
          LT ->
            -- trace "B won" $
            state
              { turn = succ $ turn state
              , aCards = ac
              , bCards = bc ++ temp state ++ [a, b]
              , lastWinner = B
              , temp = []
              }
          EQ ->
            state
              { turn = succ $ turn state
              , aCards = ac
              , bCards = bc
              , lastWinner = lastWinner state
              , temp = [a, b]
              }

average :: [Either State (Side, Int)] -> Statistics
average results =
  ( (winningQuota A, winningQuota B)
  , quota $ sum $ map snd $ rights results -- average
  , length $ lefts results
  )
  where
    quota x = fromIntegral x / fromIntegral (length results)
    winningQuota side = quota $ length $ filter ((== side) . fst) (rights results)

allTheStats :: Int -> [Int] -> [Int] -> IO [(Parameters, Statistics)]
allTheStats games possibleFields possibleCardsPerSide =
  mapConcurrently averageGame allTheGames
  where allTheGames = Parameters <$> [A, B] <*> possibleFields <*> possibleCardsPerSide
        averageGame parameters = (,) parameters . average <$> replicateM games (runGame <$> initialState parameters)

renderStats :: (Parameters, Statistics) -> String
renderStats (parameters, ((winsA, winsB), turns, loops)) = printf "(begin %s fields %4d cards %4d) A %.2f B %.2f Turns %3.1f Loops %d" (show $ beginningSide parameters) (fields parameters) (cardsPerSide parameters) winsA winsB turns loops

main = do
  stats <- allTheStats 100 [6] [1..20]
  mapM_ (putStrLn . renderStats) stats

-- how many turns do games need on average
-- is any of that influenced by the number of fields, the number of cards per side and who began (initial lastWinner)
--
