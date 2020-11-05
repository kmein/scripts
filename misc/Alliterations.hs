{-# LANGUAGE OverloadedStrings #-}

import Control.Arrow ((>>>), (&&&), (***))
import Data.Char (isPunctuation)
import Data.List
import Options.Applicative
import Text.Printf
import qualified Data.Text as T 
import qualified Data.Text.IO as T

import Stopwords

slidingWindow :: Int -> [a] -> [[a]]
slidingWindow n xs 
  | n > length xs = []
  | otherwise = take n xs : slidingWindow n (tail xs)

alliterations :: Language -> T.Text -> [T.Text]
alliterations language = 
  cleanUp language
  >>> filter containsAlliteration >>>
  map (T.intercalate " ") 

cleanUp language = 
  T.filter (not.isPunctuation) 
  >>> T.toLower 
  >>> T.words 
  >>> filter (`notElem` stopwords language) 
  >>> slidingWindow 2 

containsAlliteration :: [T.Text] -> Bool
containsAlliteration = map T.head >>> ((nub >>> length) &&& length) >>> uncurry (<)

measure :: Num a => Language -> T.Text -> (a, a)
measure language = (alliterations language &&& cleanUp language) >>> (genericLength *** genericLength) 

data Mode = Alliterations | Statistics

mode :: Parser (Language, Mode)
mode = 
  (,) 
  <$> ((English <$ switch (long "english" <> help "Cut out english stopwords"))
      <|> (German <$ switch (long "german" <> help "Cut out german stopwords"))
      <|> pure NoLanguage)
  <*> ((Alliterations <$ switch (long "alliterations" <> short 'a' <> help "List all alliterations"))
      <|> (Statistics <$ switch (long "statistics" <> short 's' <> help "Show statistics (quota)")))

main :: IO ()
main = execParser options >>= \(language, mode) -> 
  case mode of
    Alliterations ->
      T.interact $ T.unlines . alliterations language 
    Statistics -> T.interact $ \text ->
      let (als, ws) = measure language text :: (Int, Int)
       in T.pack (printf "Bigrams found: %u\nAlliterations found: %u\n\n%.2f%%\n" ws als (100 * fromIntegral als/fromIntegral ws :: Double))
  where options = info (mode <**> helper) (fullDesc <> progDesc "finds alliterations and there density in German texts")
