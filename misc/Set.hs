{-# LANGUAGE TypeSynonymInstances, TypeFamilies #-}
module Set where

import Data.Char
import Data.Complex
import qualified GHC.Exts as Exts

import Prelude hiding (filter)
import qualified Prelude
import Data.Functor.Contravariant
import qualified Data.Functor.Contravariant.Divisible as Divisible
import Data.Isomorphism

liftP :: (Bool -> Bool) -> Predicate a -> Predicate a
liftP op p = Predicate $ op <$> getPredicate p

liftP2 :: (Bool -> Bool -> Bool) -> Predicate a -> Predicate a -> Predicate a
liftP2 op p q = Predicate $ op <$> getPredicate p <*> getPredicate q

type Set = Predicate

instance (Eq a) => Exts.IsList (Set a) where
    type Item (Set a) = a
    fromList = fromFoldable
    toList = undefined

(\\) :: Set a -> Set a -> Set a
(\\) = difference

(\/) :: Set a -> Set a -> Set a
(\/) = union

(/\) :: Set a -> Set a -> Set a
(/\) = intersection

cantor :: (a -> Set a) -> Set a
cantor = Predicate . (<*>) notMember

cartesian :: Set a -> Set b -> Set (a, b)
cartesian = Divisible.divided

complement :: Set a -> Set a
complement = liftP not

containing :: a -> Set (Set a)
containing = Predicate . member

delete :: (Eq a) => a -> Set a -> Set a
delete x m = m \\ singleton x

difference :: Set a -> Set a -> Set a
difference = liftP2 (\x y -> x && not y)

disjointUnion :: Set a -> Set b -> Set (Either a b)
disjointUnion = Divisible.chosen

empty :: Set a
empty = Predicate $ const False

universal :: Set a
universal = mempty

filter :: (a -> Bool) -> Set a -> Set a
filter f m = Predicate $ (&&) <$> getPredicate m <*> f

fromFoldable :: (Foldable t, Eq a) => t a -> Set a
fromFoldable = Predicate . flip elem

insert :: (Eq a) => a -> Set a -> Set a
insert = union . singleton

intersection :: Set a -> Set a -> Set a
intersection = liftP2 (&&)

member :: a -> Set a -> Bool
member = flip getPredicate

notMember :: a -> Set a -> Bool
notMember = (not .) . member

partition :: (a -> Bool) -> Set a -> (Set a, Set a)
partition f m = (filter f m, filter (not . f) m)

singleton :: (Eq a) => a -> Set a
singleton = Predicate . (==)

split :: (Ord a) => a -> Set a -> (Set a, Set a)
split pivot m = (filter (< pivot) m, filter (> pivot) m)

splitMember :: (Ord a) => a -> Set a -> (Set a, Bool, Set a)
splitMember pivot m = let (ls, gs) = split pivot m in (ls, member pivot m, gs)

toList :: (Enum a, Bounded a) => Set a -> [a]
toList m = Prelude.filter (getPredicate m) [minBound .. maxBound]

union :: Set a -> Set a -> Set a
union = liftP2 (||)

unions :: (Foldable t) => t (Set a) -> Set a
unions = foldr union empty

naturals :: (Ord a, Integral a) => Set a
naturals = Predicate (>= 0)

reals :: (Real a) => Set a
reals = universal

rationals :: (Fractional a) => Set a
rationals = universal

integers :: (Integral a) => Set a
integers = universal

complexes :: (Real a) => Set (Complex a)
complexes = universal

mapIso :: Iso (->) a b -> Set a -> Set b
mapIso = contramap . project

digit :: Iso (->) Int Char
digit = Iso intToDigit digitToInt

casePreserving :: (String -> String) -> String -> String
casePreserving f = applyCasing <$> map isUpper <*> f
    where applyCasing = zipWith (bool toUpper toLower)
          bool x y b = if b then x else y

main = do
  let sampleSet = fromFoldable [0 .. 9]
  let digitSet = mapIso digit sampleSet
  print (toList digitSet)

shorten :: String -> String
shorten word
  | n <= 10 = word
  | otherwise = [head word] ++ show (n - 2) ++ [last word]
  where
    n = length word

lev :: (Eq a) => [a] -> [a] -> Int
lev [] ys = length ys
lev xs [] = length xs
lev xxs@(x:xs) yys@(y:ys)
  | x == y = lev xs ys
  | otherwise = 1 + minimum [lev xs yys, lev xxs ys, lev xs ys]
