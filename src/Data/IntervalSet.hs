{-# LANGUAGE LambdaCase #-}
module Data.IntervalSet
( IntervalSet(..)
, empty
, singleton
, fromList
, bounds
, null
, insert
, split
, before
  -- * Re-exports
, Interval(..)
) where

import           Data.Coerce (coerce)
import qualified Data.FingerTree as F
import qualified Data.Foldable as Foldable (foldl', toList)
import           Data.Function (on)
import           Data.Functor.Classes (showsUnaryWith)
import           Data.Functor.I
import           Data.Functor.Interval
import           Prelude hiding (null)

newtype IntervalSet a = IntervalSet { getIntervalSet :: F.FingerTree (Maybe (Interval I a)) (Interval I a) }

instance Eq a => Eq (IntervalSet a) where (==) = (==) `on` toList

instance Ord a => Ord (IntervalSet a) where compare = compare `on` toList

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
insert inserted t = l >< go inserted (getIntervalSet r)
  where
  (l, r) = split (before inserted) t
  go inserted s = case F.viewl s of
    F.EmptyL -> singleton inserted
    h F.:< t
      | sup inserted < inf h -> inserted <| IntervalSet s
      | otherwise            -> go (inserted <> h) t

before :: Ord a => Interval I a -> Interval I a -> Bool
before inserted i = inf inserted <= sup i


-- Internal

split :: Ord a => (Interval I a -> Bool) -> IntervalSet a -> (IntervalSet a, IntervalSet a)
split p = coerce . F.split (maybe False p) . getIntervalSet

(<|) :: Ord a => Interval I a -> IntervalSet a -> IntervalSet a
(<|) = coerce ((F.<|) :: Ord a => Interval I a -> F.FingerTree (Maybe (Interval I a)) (Interval I a) -> F.FingerTree (Maybe (Interval I a)) (Interval I a))

infixr 5 <|

(><) :: Ord a => IntervalSet a -> IntervalSet a -> IntervalSet a
(><) = coerce ((F.><) :: Ord a => F.FingerTree (Maybe (Interval I a)) (Interval I a) -> F.FingerTree (Maybe (Interval I a)) (Interval I a) -> F.FingerTree (Maybe (Interval I a)) (Interval I a))

infixr 5 ><
