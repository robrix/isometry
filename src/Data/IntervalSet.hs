module Data.IntervalSet
( IntervalSet
, empty
, singleton
, fromList
, bounds
, null
, toList
, insert
, delete
, splitAround
  -- * Re-exports
, Interval(..)
) where

import qualified Data.FingerTree as F
import qualified Data.Foldable as Foldable (foldl', toList)
import           Data.Functor.Classes (showsUnaryWith)
import           Data.Coerce
import           Data.Functor.I
import           Data.Functor.Interval
import           Prelude hiding (null)

newtype IntervalSet f a = IntervalSet { getIntervalSet :: F.FingerTree (Maybe (Interval f a)) (Interval f a) }
  deriving (Eq, Ord)

instance Show (f a) => Show (IntervalSet f a) where
  showsPrec p = showsUnaryWith showsPrec "fromList" p . toList

empty :: Ord a => IntervalSet I a
empty = IntervalSet F.empty

singleton :: Ord a => Interval I a -> IntervalSet I a
singleton = IntervalSet . F.singleton

fromList :: Ord a => [Interval I a] -> IntervalSet I a
fromList = Foldable.foldl' (flip insert) empty


bounds :: Ord a => IntervalSet I a -> Maybe (Interval I a)
bounds = F.measure . getIntervalSet

null :: IntervalSet I a -> Bool
null = F.null . getIntervalSet

toList :: IntervalSet f a -> [Interval f a]
toList = Foldable.toList . getIntervalSet


insert :: Ord a => Interval I a -> IntervalSet I a -> IntervalSet I a
insert inserted t = l >< maybe inserted (union inserted) (bounds m) <| r
  where
  (l, m, r) = splitAround inserted t

delete :: Ord a => Interval I a -> IntervalSet I a -> IntervalSet I a
delete deleted t = l >< r'
  where
  (l, m, r) = splitAround deleted t
  r' = case bounds m of
    Just h
      | inf h < inf deleted
      , sup deleted < sup h -> Interval (inf h) (inf deleted) <| Interval (sup deleted) (sup h) <| r
      | inf h < inf deleted -> Interval (inf h) (inf deleted) <| r
      | sup deleted < sup h -> Interval (sup deleted) (sup h) <| r
    _ -> r

splitAround :: Ord a => Interval I a -> IntervalSet I a -> (IntervalSet I a, IntervalSet I a, IntervalSet I a)
splitAround i (IntervalSet s) = (IntervalSet l, IntervalSet n', IntervalSet r')
  where
  (l, m) = F.split (maybe False (before i)) s
  (n, r) = F.split (maybe False (after  i)) m
  (n', r') = case F.viewl r of
    F.EmptyL -> (n, r)
    h F.:< t
      | sup i < inf h -> (n, r)
      | otherwise     -> (n F.|> h, t)

before :: Ord a => Interval I a -> Interval I a -> Bool
before subject i = inf subject <= sup i

after :: Ord a => Interval I a -> Interval I a -> Bool
after subject i = sup subject < sup i


-- Internal

infixr 5 ><, <|

(><) :: Ord a => IntervalSet I a -> IntervalSet I a -> IntervalSet I a
(><) = coerce ((F.><) :: Ord a => F.FingerTree (Maybe (Interval I a)) (Interval I a) -> F.FingerTree (Maybe (Interval I a)) (Interval I a) -> F.FingerTree (Maybe (Interval I a)) (Interval I a))

(<|) :: Ord a => Interval I a -> IntervalSet I a -> IntervalSet I a
(<|) = coerce ((F.<|) :: Ord a => Interval I a -> F.FingerTree (Maybe (Interval I a)) (Interval I a) -> F.FingerTree (Maybe (Interval I a)) (Interval I a))
