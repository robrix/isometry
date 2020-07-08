{-# LANGUAGE LambdaCase #-}
module Data.IntervalSet
( IntervalSet()
, empty
, singleton
, fromList
, bounds
, null
, insert
, split
  -- * Re-exports
, Interval(..)
) where

import Data.Foldable (foldl')
import Data.Function (on)
import Data.Functor.Classes (showsUnaryWith)
import Data.Functor.I
import Data.Functor.Interval
import Prelude hiding (null)

data IntervalSet a
  = Empty
  | Node (Interval I a) (IntervalSet a) (Interval I a) (IntervalSet a)

instance Eq a => Eq (IntervalSet a) where (==) = (==) `on` toList

instance Ord a => Ord (IntervalSet a) where compare = compare `on` toList

instance Show a => Show (IntervalSet a) where
  showsPrec p = showsUnaryWith showsPrec "fromList" p . toList

empty :: IntervalSet a
empty = Empty

singleton :: Interval I a -> IntervalSet a
singleton i = Node i Empty i Empty

fromList :: Ord a => [Interval I a] -> IntervalSet a
fromList = foldl' (flip insert) empty


bounds :: IntervalSet a -> Maybe (Interval I a)
bounds Empty          = Nothing
bounds (Node b _ _ _) = Just b

null :: IntervalSet a -> Bool
null Empty = True
null _     = False

toList :: IntervalSet a -> [Interval I a]
toList = ($ []) . go id
  where
  go f = \case
    Empty -> f
    Node _ l i r -> go f l . (i:) . go f r


insert :: Ord a => Interval I a -> IntervalSet a -> IntervalSet a
insert inserted t = l >< inserted <| r
  where
  (l, m) = split before t
  (_, r) = split after m
  before i = sup inserted <= inf i
  after  i = sup inserted <  inf i


-- Internal

split :: Ord a => (Interval I a -> Bool) -> IntervalSet a -> (IntervalSet a, IntervalSet a)
split _ Empty          = (Empty, Empty)
split p (Node b l i r)
  | p b = (Empty, Node b l i r)
  | p i, (ll, lr) <- split p l = (ll, lr >< i <| r)
  | otherwise, (rl, rr) <- split p r = ((l |> i) >< rl, rr)

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
