{-# LANGUAGE LambdaCase #-}
module Data.IntervalSet
( IntervalSet()
, empty
, singleton
, bounds
, null
, insert
  -- * Re-exports
, Interval(..)
) where

import Data.Functor.I
import Data.Functor.Interval
import Prelude hiding (null)

data IntervalSet a
  = Empty
  | Node (Interval I a) (IntervalSet a) (Interval I a) (IntervalSet a)
  deriving (Eq, Ord, Show)

empty :: IntervalSet a
empty = Empty

singleton :: Interval I a -> IntervalSet a
singleton i = Node i Empty i Empty


bounds :: IntervalSet a -> Maybe (Interval I a)
bounds Empty          = Nothing
bounds (Node b _ _ _) = Just b

null :: IntervalSet a -> Bool
null Empty = True
null _     = False


insert :: Ord a => Interval I a -> IntervalSet a -> IntervalSet a
insert new = \case
  Empty -> singleton new
  Node b l i r
    | b `isSubintervalOf` new -> singleton new
    | sup new < inf b -> Node (new <> b) Empty new (Node b l i r)
    | sup b < inf new -> Node (b <> new) (Node b l i r) new Empty
