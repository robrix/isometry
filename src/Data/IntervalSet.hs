module Data.IntervalSet
( IntervalSet
, empty
, singleton
, fromList
, bounds
, null
, toList
, insert
, delete
, splitAround
  -- * Re-exports
, Interval(..)
) where

import qualified Data.FingerTree as F
import qualified Data.Foldable as Foldable (foldl', toList)
import           Data.Functor.Classes (showsUnaryWith)
import           Data.Coerce
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
  (l, r) = F.split (maybe False (before inserted)) t
  go inserted s = case F.viewl s of
    F.EmptyL -> F.singleton inserted
    h F.:< t
      | sup inserted < inf h -> inserted F.<| s
      | otherwise            -> go (inserted <> h) t

delete :: Ord a => Interval I a -> IntervalSet a -> IntervalSet a
delete deleted t = l >< r'
  where
  (l, m, r) = splitAround deleted t
  r' = case bounds m of
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
    h F.:< t
      | sup i < inf h -> (n, r)
      | otherwise     -> (n F.|> h, t)

before :: Ord a => Interval I a -> Interval I a -> Bool
before subject i = inf subject <= sup i

after :: Ord a => Interval I a -> Interval I a -> Bool
after subject i = sup subject < sup i


-- Internal

infixr 5 ><, <|

(><) :: Ord a => IntervalSet a -> IntervalSet a -> IntervalSet a
(><) = coerce ((F.><) :: Ord a => F.FingerTree (Maybe (Interval I a)) (Interval I a) -> F.FingerTree (Maybe (Interval I a)) (Interval I a) -> F.FingerTree (Maybe (Interval I a)) (Interval I a))

(<|) :: Ord a => Interval I a -> IntervalSet a -> IntervalSet a
(<|) = coerce ((F.<|) :: Ord a => Interval I a -> F.FingerTree (Maybe (Interval I a)) (Interval I a) -> F.FingerTree (Maybe (Interval I a)) (Interval I a))
