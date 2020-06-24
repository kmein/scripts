module Util where

import           Data.List                      ( isPrefixOf )

replaceSublist :: Eq a => [a] -> [a] -> [a] -> [a]
replaceSublist _    _           []           = []
replaceSublist list replacement xss@(x : xs) = if list `isPrefixOf` xss
  then replacement ++ replaceSublist list replacement (drop (length list) xss)
  else x : replaceSublist list replacement xs

