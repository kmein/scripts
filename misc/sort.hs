{-# LANGUAGE LambdaCase #-}
import           Control.Monad                  ( forM_ )
import           Data.Char                      ( toLower )
import           System.Environment             ( getArgs )

insertionSortM :: Monad f => (a -> a -> f Ordering) -> [a] -> f [a]
insertionSortM cmp = foldr ((=<<) . insertByM cmp) (pure [])
 where
  insertByM cmp x = \case
    []           -> pure [x]
    yys@(y : ys) -> cmp x y >>= \case
      GT -> (y :) <$> insertByM cmp x ys
      _  -> pure (x : yys)

ask :: Show a => a -> a -> IO Ordering
ask a b = do
  putStr (show a ++ " > " ++ show b ++ "? (y/n) ")
  map toLower <$> getLine >>= \case
    'y' : _ -> return GT
    _       -> return LT

main :: IO ()
main = do
  argv   <- getArgs
  sorted <- insertionSortM ask argv
  forM_ (zip [1 ..] sorted)
    $ \(place, thing) -> putStrLn (show place ++ ". " ++ show thing)
