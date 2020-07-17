{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.IntervalSet
( IntervalSet
, empty
, singleton
, fromList
, F.Measured(..)
, null
, toList
, insert
, delete
, splitAround
  -- * Re-exports
, Interval(..)
) where

import           Control.Applicative (liftA2)
import           Data.Coerce
import qualified Data.FingerTree as F
import qualified Data.Foldable as Foldable (foldl', toList)
import           Data.Functor.Classes (showsUnaryWith)
import           Data.Functor.Interval
import           Prelude hiding (null)

newtype IntervalSet f a = IntervalSet { getIntervalSet :: F.FingerTree (Maybe (Interval f a)) (Leaf f a) }
  deriving (Eq, Ord)

instance Show (f a) => Show (IntervalSet f a) where
  showsPrec p = showsUnaryWith showsPrec "fromList" p . toList

instance (Applicative f, Ord a) => F.Measured (Maybe (Interval f a)) (IntervalSet f a) where
  measure = F.measure . getIntervalSet

empty :: (Applicative f, Ord a) => IntervalSet f a
empty = IntervalSet F.empty

singleton :: (Applicative f, Ord a) => Interval f a -> IntervalSet f a
singleton = IntervalSet . F.singleton . Leaf

fromList :: (Applicative f, Foldable f, Ord a) => [Interval f a] -> IntervalSet f a
fromList = Foldable.foldl' (flip insert) empty


null :: IntervalSet f a -> Bool
null = F.null . getIntervalSet

toList :: IntervalSet f a -> [Interval f a]
toList = coerce . Foldable.toList . getIntervalSet


insert :: (Applicative f, Foldable f, Ord a) => Interval f a -> IntervalSet f a -> IntervalSet f a
insert inserted t = l >< maybe inserted (union inserted) (F.measure m) <| r
  where
  (l, m, r) = splitAround inserted t

delete :: (Applicative f, Foldable f, Ord a) => Interval f a -> IntervalSet f a -> IntervalSet f a
delete deleted t = l >< r'
  where
  (l, m, r) = splitAround deleted t
  r' = case F.measure m of
    Just h
      | inf h `lt` inf deleted
      , sup deleted `lt` sup h -> Interval (inf h) (inf deleted) <| Interval (sup deleted) (sup h) <| r
      | inf h `lt` inf deleted -> Interval (inf h) (inf deleted) <| r
      | sup deleted `lt` sup h -> Interval (sup deleted) (sup h) <| r
    _ -> r

splitAround :: (Applicative f, Foldable f, Ord a) => Interval f a -> IntervalSet f a -> (IntervalSet f a, IntervalSet f a, IntervalSet f a)
splitAround i (IntervalSet s) = (IntervalSet l, IntervalSet n', IntervalSet r')
  where
  (l, m) = F.split (maybe False (before i)) s
  (n, r) = F.split (maybe False (after  i)) m
  (n', r') = case F.viewl r of
    F.EmptyL -> (n, r)
    Leaf h F.:< t
      | sup i `lt` inf h -> (n, r)
      | otherwise        -> (n F.|> Leaf h, t)


-- Internal

infixr 5 ><, <|

(><) :: forall f a . (Applicative f, Ord a) => IntervalSet f a -> IntervalSet f a -> IntervalSet f a
(><) = coerce ((F.><) :: F.FingerTree (Maybe (Interval f a)) (Leaf f a) -> F.FingerTree (Maybe (Interval f a)) (Leaf f a) -> F.FingerTree (Maybe (Interval f a)) (Leaf f a))

(<|) :: forall f a . (Applicative f, Ord a) => Interval f a -> IntervalSet f a -> IntervalSet f a
(<|) = coerce ((F.<|) :: Leaf f a -> F.FingerTree (Maybe (Interval f a)) (Leaf f a) -> F.FingerTree (Maybe (Interval f a)) (Leaf f a))


newtype Leaf f a = Leaf { getLeaf :: Interval f a }
  deriving (Eq, Ord)

instance (Applicative f, Ord a) =>  F.Measured (Maybe (Interval f a)) (Leaf f a) where
  measure = Just . getLeaf


before, after :: (Applicative f, Foldable f, Ord a) => Interval f a -> Interval f a -> Bool
before a b = inf a `lte` sup b
after  a b = sup a `lt`  sup b

liftRelation :: (Applicative f, Foldable f) => (a -> b -> Bool) -> f a -> f b -> Bool
liftRelation rel a b = and (liftA2 rel a b)

infix 4 `lt`, `lte`

lt :: (Applicative f, Foldable f, Ord a) => f a -> f a -> Bool
lt = liftRelation (<)

lte :: (Applicative f, Foldable f, Ord a) => f a -> f a -> Bool
lte = liftRelation (<=)
