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
    | sup new < inf b -> new <| Node b l i r
    | sup b < inf new -> Node b l i r |> new
    | sup new < inf i -> Node (b `union` new) (insert new l) i r
    | sup i < inf new -> Node (b `union` new) l i (insert new r)
    | new `isSubintervalOf` i -> Node b l i r


-- Internal

(<|) :: Ord a => Interval I a -> IntervalSet a -> IntervalSet a
i <| t = singleton i >< t

infixr 5 <|

(|>) :: Ord a => IntervalSet a -> Interval I a -> IntervalSet a
t |> i = t >< singleton i

infixl 5 |>

(><) :: Ord a => IntervalSet a -> IntervalSet a -> IntervalSet a
Empty        >< r = r
Node b l i m >< r = Node (maybe b (union b) (bounds r)) l i (m >< r)

infixr 5 ><
