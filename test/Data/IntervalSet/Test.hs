{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module Data.IntervalSet.Test
( tests
) where

import           Control.Monad (join)
import           Data.Functor.I
import           Data.Functor.Interval
import           Data.IntervalSet as I
import           Data.Maybe (fromMaybe)
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

tests :: IO Bool
tests = checkParallel $$(discover)

prop_empty_null = withTests 1 . property $ I.null (empty @I @Int) === True


prop_insert_idempotence = property $ do
  i <- forAll gi
  s <- insert i <$> forAll gs
  insert i s === s

prop_insert_monotonicity = property $ do
  i <- forAll gi
  s <- forAll gs
  assert . fromMaybe True $ isSubintervalOf <$> measure s <*> measure (insert i s)
  assert . fromMaybe True $ (i `isSubintervalOf`) <$> measure (insert i s)


prop_delete_inverse = property $ do
  s <- forAll gs
  i <- forAll gi
  insert i (delete i s) === insert i s

prop_delete_annihilation = property $ do
  s <- forAll gs
  maybe id delete (measure s) s === empty


prop_fromList_inverse = property $ do
  s <- forAll gs
  fromList (toList s) === s


prop_toList_inverse = property $ do
  s <- reverse . foldr mkDisjoint [] <$> forAll (Gen.list (Range.linear 0 100) gi)
  toList (fromList s) === s
  where
  mkDisjoint i [] = [i]
  mkDisjoint i is = mapInterval (+ succ (sup (head is))) i:is


prop_splitAround_infimum = property $ do
  s <- forAll gs
  i <- forAll gi
  let (l, _, _) = splitAround i s
  all ((< inf i) . sup) (toList l) === True

prop_splitAround_supremum = property $ do
  s <- forAll gs
  i <- forAll gi
  let (_, _, r) = splitAround i s
  all ((sup i <) . inf) (toList r) === True

prop_splitAround_intersects = property $ do
  s <- forAll gs
  i <- forAll gi
  let (_, c, _) = splitAround i s
  all (intersects i) (toList c) === True


prop_intervalSet_coverage = property $ do
  s <- forAll gs
  let is = I.toList s
  cover 10 "empty" (I.null s)
  cover 10 "singleton" (length is == 1)
  cover 10 "disjoint" (length is > 1)
  cover 10 "point" (maybe False isPoint (measure s))
  cover 10 "span" (maybe False (not . isPoint) (measure s))


gp = Gen.int (Range.linear 0 100)
gi = interval gp
gs = intervalSet gi

intervalSet :: (MonadGen m, Ord a) => m (Interval I a) -> m (IntervalSet I a)
intervalSet i = Gen.choice
  [ pure empty
  , singleton <$> i
  , fromList <$> Gen.list (Range.linear 0 100) i
  ]

interval :: (MonadGen m, Num a) => m a -> m (Interval I a)
interval p = Gen.choice
  [ join (...) <$> p
  , mk <$> p <*> p
  ]
  where
  mk a b = a ... a + b + 1
