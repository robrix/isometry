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

data IntervalSet a
  = Empty
  | Branch (Interval I a) (IntervalSet a) (Interval I a) (IntervalSet a)

empty :: IntervalSet a
empty = Empty

singleton :: Interval I a -> IntervalSet a
singleton i = Branch i empty i empty


bounds :: IntervalSet a -> Maybe (Interval I a)
bounds Empty            = Nothing
bounds (Branch b _ _ _) = Just b


insert :: Ord a => Interval I a -> IntervalSet a -> IntervalSet a
insert i = \case
  Empty -> singleton i
  Branch b l i' g
    | i `isSubintervalOf` i' -> Branch b l i' g
    | sup i < inf i'         -> Branch b' (insert i l) i' g
    | inf i < sup i'         -> Branch b' l i' (insert i g)
    | otherwise              -> merge b' l (i `union` i') g
    where
    b' = i `union` b

merge :: Interval I a -> IntervalSet a -> Interval I a -> IntervalSet a -> IntervalSet a
merge = Branch
