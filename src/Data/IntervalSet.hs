module Data.IntervalSet
( IntervalSet(IntervalSet)
, empty
, singleton
, bounds
, null
, insert
, split
, larger
, smaller
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
insert new set
  | null set  = singleton new
  | otherwise = IntervalSet $ case bounds lt of
    Nothing -> new <| getIntervalSet gt
    Just l
      | l `isSubintervalOf` new -> new <| getIntervalSet gt
      | sup l < inf new         -> getIntervalSet lt >< new <| getIntervalSet gt
      | otherwise               -> case split (smaller new) lt of
        (lt', t) -> getIntervalSet lt' >< maybe new (union new) (bounds t) <| getIntervalSet gt
    where
    (lt, gt) = split (larger new) set

split :: Ord a => (Maybe (Interval I a) -> Bool) -> IntervalSet a -> (IntervalSet a, IntervalSet a)
split p (IntervalSet set) = let (lt, gt) = F.split p set in (IntervalSet lt, IntervalSet gt)

larger, smaller :: Ord a => Interval I a -> Maybe (Interval I a) -> Bool
larger  a = maybe False (\ b -> sup a < sup b)
smaller a = maybe False (\ b -> inf a >= sup b)
