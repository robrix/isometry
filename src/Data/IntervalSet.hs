{-# LANGUAGE LambdaCase #-}
module Data.IntervalSet
( IntervalSet(IntervalSet)
, empty
, singleton
, bounds
, insert
  -- * Re-exports
, Interval(..)
) where

import qualified Data.FingerTree as F
import           Data.Functor.I
import           Data.Functor.Interval

newtype IntervalSet a = IntervalSet { getIntervalSet :: F.FingerTree (Maybe (Interval I a)) (Interval I a) }

empty :: Ord a => IntervalSet a
empty = IntervalSet F.empty

singleton :: Ord a => Interval I a -> IntervalSet a
singleton i = IntervalSet $ F.singleton i


bounds :: Ord a => IntervalSet a -> Maybe (Interval I a)
bounds = F.measure . getIntervalSet


insert :: Ord a => Interval I a -> IntervalSet a -> IntervalSet a
insert i (IntervalSet set) = IntervalSet (lt F.>< i F.<| gt)
  where
  (lt, gt) = F.split (\case
    Just i' -> sup i' < inf i
    Nothing -> False) set
