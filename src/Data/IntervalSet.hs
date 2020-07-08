{-# LANGUAGE LambdaCase #-}
module Data.IntervalSet
( IntervalSet(..)
, empty
, singleton
, fromList
, bounds
, null
, insert
  -- * Re-exports
, Interval(..)
) where

import qualified Data.FingerTree as F
import qualified Data.Foldable as Foldable (foldl', toList)
import           Data.Functor.Classes (showsUnaryWith)
import           Data.Functor.I
import           Data.Functor.Interval
import           Prelude hiding (null)

newtype IntervalSet a = IntervalSet { getIntervalSet :: F.FingerTree (Maybe (Interval I a)) (Interval I a) }
  deriving (Eq, Ord)

instance Show a => Show (IntervalSet a) where
  showsPrec p = showsUnaryWith showsPrec "fromList" p . toList

empty :: Ord a => IntervalSet a
empty = IntervalSet F.empty

singleton :: Ord a => Interval I a -> IntervalSet a
singleton = IntervalSet . F.singleton

fromList :: Ord a => [Interval I a] -> IntervalSet a
fromList = Foldable.foldl' (flip insert) empty


bounds :: Ord a => IntervalSet a -> Maybe (Interval I a)
bounds = F.measure . getIntervalSet

null :: IntervalSet a -> Bool
null = F.null . getIntervalSet

toList :: IntervalSet a -> [Interval I a]
toList = Foldable.toList . getIntervalSet


insert :: Ord a => Interval I a -> IntervalSet a -> IntervalSet a
insert inserted (IntervalSet t) = IntervalSet $ l F.>< go inserted r
  where
  (l, r) = F.split (maybe False before) t
  go inserted s = case F.viewl s of
    F.EmptyL -> F.singleton inserted
    h F.:< t
      | sup inserted < inf h -> inserted F.<| s
      | otherwise            -> go (inserted <> h) t
  before i = inf inserted <= sup i
