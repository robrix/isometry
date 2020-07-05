{-# LANGUAGE LambdaCase #-}
module Data.IntervalSet
( IntervalSet(IntervalSet)
, empty
, singleton
, bounds
, null
, insert
  -- * Re-exports
, Interval(..)
) where

import qualified Data.FingerTree as F
import           Data.Functor.I
import           Data.Functor.Interval
import           Prelude hiding (null)

newtype IntervalSet a = IntervalSet { getIntervalSet :: F.FingerTree (Maybe (Interval I a)) (Interval I a) }
  deriving (Eq, Ord, Show)

empty :: Ord a => IntervalSet a
empty = IntervalSet F.empty

singleton :: Ord a => Interval I a -> IntervalSet a
singleton i = IntervalSet $ F.singleton i


bounds :: Ord a => IntervalSet a -> Maybe (Interval I a)
bounds = F.measure . getIntervalSet

null :: IntervalSet a -> Bool
null = F.null . getIntervalSet


insert :: Ord a => Interval I a -> IntervalSet a -> IntervalSet a
insert i set
  | null set  = singleton i
  | otherwise = IntervalSet (lt F.>< go gt)
  where
  (lt, gt) = F.split (\case
    Just i' -> sup i' < inf i
    Nothing -> False) (getIntervalSet set)
  go gt = i F.<| gt
