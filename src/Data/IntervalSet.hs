module Data.IntervalSet
( IntervalSet(IntervalSet)
, empty
, singleton
, bounds
, null
, insert
, larger
, smaller
  -- * Re-exports
, Interval(..)
) where

import           Data.FingerTree hiding (empty, null, singleton)
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
  | otherwise = IntervalSet (go (getIntervalSet set))
  where
  go set = case measure lt of
    Nothing -> new <| gt
    Just l
      | l `isSubintervalOf` new -> new <| gt
      | sup l < inf new         -> lt >< new <| gt
      | otherwise               -> case split (smaller new) lt of
        (lt', t) -> lt' >< maybe new (union new) (measure t) <| gt
    where
    (lt, gt) = split (larger new) set

larger, smaller :: Ord a => Interval I a -> Maybe (Interval I a) -> Bool
larger  new = maybe False ((> sup new) . sup)
smaller new = maybe False ((<= inf new) . sup)
