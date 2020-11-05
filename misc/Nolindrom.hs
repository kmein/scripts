module Nolindrom where

nolindrom :: Int -> Integer -> Bool
nolindrom i n
  | i >= 100 = True
  | otherwise =
    let r = read . reverse . show $ n
        n' = r + n
     in not (palindrome n') && nolindrom (succ i) n'
  where
    palindrome x =
      let s = show x
       in s == reverse s

