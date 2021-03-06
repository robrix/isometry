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

import           Data.Coerce
import qualified Data.FingerTree as F
import qualified Data.Foldable as Foldable (foldl', toList)
import           Data.Functor.Classes (showsUnaryWith)
import           Data.Functor.I
import           Data.Functor.Interval
import           Prelude hiding (null)

newtype IntervalSet a = IntervalSet { getIntervalSet :: F.FingerTree (Maybe (Interval I a)) (Leaf a) }
  deriving (Eq, Ord)

instance Show a => Show (IntervalSet a) where
  showsPrec p = showsUnaryWith showsPrec "fromList" p . toList

instance Ord a => F.Measured (Maybe (Interval I a)) (IntervalSet a) where
  measure = F.measure . getIntervalSet

empty :: Ord a => IntervalSet a
empty = IntervalSet F.empty

singleton :: Ord a => Interval I a -> IntervalSet a
singleton = IntervalSet . F.singleton . Leaf

fromList :: Ord a => [Interval I a] -> IntervalSet a
fromList = Foldable.foldl' (flip insert) empty


null :: IntervalSet a -> Bool
null = F.null . getIntervalSet

toList :: IntervalSet a -> [Interval I a]
toList = coerce . Foldable.toList . getIntervalSet


insert :: Ord a => Interval I a -> IntervalSet a -> IntervalSet a
insert inserted t = l >< maybe inserted (union inserted) (F.measure m) <| r
  where
  (l, m, r) = splitAround inserted t

delete :: Ord a => Interval I a -> IntervalSet a -> IntervalSet a
delete deleted t = l >< r'
  where
  (l, m, r) = splitAround deleted t
  r' = case F.measure m of
    Just h
      | inf h < inf deleted
      , sup deleted < sup h -> Interval (inf h) (inf deleted) <| Interval (sup deleted) (sup h) <| r
      | inf h < inf deleted -> Interval (inf h) (inf deleted) <| r
      | sup deleted < sup h -> Interval (sup deleted) (sup h) <| r
    _ -> r

splitAround :: Ord a => Interval I a -> IntervalSet a -> (IntervalSet a, IntervalSet a, IntervalSet a)
splitAround i (IntervalSet s) = (IntervalSet l, IntervalSet n', IntervalSet r')
  where
  (l, m) = F.split (maybe False (before i)) s
  (n, r) = F.split (maybe False (after  i)) m
  (n', r') = case F.viewl r of
    F.EmptyL -> (n, r)
    Leaf h F.:< t
      | sup i < inf h -> (n, r)
      | otherwise     -> (n F.|> Leaf h, t)


-- Internal

infixr 5 ><, <|

(><) :: forall a . Ord a => IntervalSet a -> IntervalSet a -> IntervalSet a
(><) = coerce ((F.><) :: F.FingerTree (Maybe (Interval I a)) (Leaf a) -> F.FingerTree (Maybe (Interval I a)) (Leaf a) -> F.FingerTree (Maybe (Interval I a)) (Leaf a))

(<|) :: forall a . Ord a => Interval I a -> IntervalSet a -> IntervalSet a
(<|) = coerce ((F.<|) :: Leaf a -> F.FingerTree (Maybe (Interval I a)) (Leaf a) -> F.FingerTree (Maybe (Interval I a)) (Leaf a))


newtype Leaf a = Leaf { getLeaf :: Interval I a }
  deriving (Eq, Ord)

instance Ord a =>  F.Measured (Maybe (Interval I a)) (Leaf a) where
  measure = Just . getLeaf


before, after :: Ord a => Interval I a -> Interval I a -> Bool
before a b = inf a <= sup b
after  a b = sup a <  sup b
