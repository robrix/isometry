{-# LANGUAGE LambdaCase #-}
module Data.IntervalSet
( IntervalSet(..)
, empty
, singleton
, bounds
, insert
  -- * Re-exports
, Interval(..)
) where

import Data.Functor.I
import Data.Functor.Interval
import Data.Maybe (fromMaybe)

data IntervalSet a
  = Empty
  | Branch (IntervalSet a) (Interval I a) (IntervalSet a)

empty :: IntervalSet a
empty = Empty

singleton :: Interval I a -> IntervalSet a
singleton a = Branch empty a empty


bounds :: Ord a => IntervalSet a -> Maybe (Interval I a)
bounds Empty          = Nothing
bounds (Branch l i g) = Just (fromMaybe i (bounds l) `union` fromMaybe i (bounds g))


insert :: Ord a => Interval I a -> IntervalSet a -> IntervalSet a
insert i = \case
  Empty -> singleton i
  Branch l i' g
    | sup i < inf i' -> Branch (insert i l) i' g
    | inf i < sup i' -> Branch l i' (insert i g)
    | otherwise      -> mkBranch l (i `union` i') g

mkBranch :: IntervalSet a -> Interval I a -> IntervalSet a -> IntervalSet a
mkBranch = Branch
