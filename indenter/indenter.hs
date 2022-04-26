#!/usr/bin/env runhaskell
import Control.Monad
import Data.Maybe
import System.Environment
import System.Random
import Text.Read

main :: IO ()
main = do
  intArgument <- (readMaybe <=< listToMaybe) <$> getArgs
  let choice xs = (xs !!) <$> randomRIO (0, length xs - 1)
      sample population = flip replicateM (choice population) =<< randomRIO (1, fromMaybe 5 intArgument)
      randomWhitespace = sample " \t\n"
  inputWords <- words <$> getContents
  putStrLn . concat =<< mapM (\x -> (x++) <$> randomWhitespace) inputWords
