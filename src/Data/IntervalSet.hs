module Data.IntervalSet
( IntervalSet(IntervalSet)
, empty
, singleton
, bounds
, null
, insert
, split
, before
, after
  -- * Re-exports
, Interval(..)
) where

import           Data.FingerTree hiding (empty, null, singleton, split)
import qualified Data.FingerTree as F
import           Data.Functor.I
import           Data.Functor.Interval
import           Prelude hiding (null)

newtype IntervalSet a = IntervalSet { getIntervalSet :: FingerTree (Maybe (Interval I a)) (Interval I a) }
  deriving (Eq, Ord, Show)

empty :: Ord a => IntervalSet a
empty = IntervalSet F.empty

singleton :: Ord a => Interval I a -> IntervalSet a
singleton i = IntervalSet $ F.singleton i


bounds :: Ord a => IntervalSet a -> Maybe (Interval I a)
bounds = measure . getIntervalSet

null :: IntervalSet a -> Bool
null = F.null . getIntervalSet


insert :: Ord a => Interval I a -> IntervalSet a -> IntervalSet a
insert new (IntervalSet set) = IntervalSet $ case F.split (before new) set of
  (lt, rest) -> lt >< case F.split (after new) rest of
    (mid, gt) -> maybe new (union new) (measure mid) <| gt

split :: Ord a => (Maybe (Interval I a) -> Bool) -> IntervalSet a -> (IntervalSet a, IntervalSet a)
split p (IntervalSet set) = let (lt, gt) = F.split p set in (IntervalSet lt, IntervalSet gt)

before :: Ord a => Interval I a -> Maybe (Interval I a) -> Bool
before a = maybe False (\ b -> inf a <= sup b)

after :: Ord a => Interval I a -> Maybe (Interval I a) -> Bool
after a = maybe False (\ b -> sup a < sup b)
