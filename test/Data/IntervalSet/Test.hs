{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module Data.IntervalSet.Test
( tests
, isDisjoint
) where

import           Control.Monad (join)
import           Data.Functor.I
import           Data.Functor.Interval
import           Data.IntervalSet as I
import           Data.Maybe (fromMaybe)
import           GHC.Stack
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
  s <- forAll (disjointIntervals gi)
  toList (fromList s) === s


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


prop_intervalSet_coverage = verifiedTermination . withConfidence (10^(9::Int)) . property $ do
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


prop_disjointIntervals_isDisjoint = property $ do
  is <- forAll (disjointIntervals gi)
  assert $ isDisjoint is

disjointIntervals :: (MonadGen m, Num a) => m (Interval I a) -> m [Interval I a]
disjointIntervals gi = reverse . foldr mkDisjoint [] <$> Gen.list (Range.linear 0 100) gi
  where
  mkDisjoint i [] = [i]
  mkDisjoint i is = mapInterval (+ (1 + sup (head is))) i:is


prop_isDisjoint_empty = withTests 1 . property $
  assert $ isDisjoint ([] :: [Interval I Int])

prop_isDisjoint_repeat = property $ do
  i <- forAll gi
  assert . not $ isDisjoint [i, i]

prop_isDisjoint_abut = property $ do
  i <- forAll gi
  assert . not $ isDisjoint [i, Interval (sup i) (sup i + diameter i)]

isDisjoint :: Ord a => [Interval I a] -> Bool
isDisjoint []             = True
isDisjoint (i:is)
  | any (intersects i) is = False
  | otherwise             = isDisjoint is


holds :: (MonadTest m, Show a, HasCallStack) => (a -> Bool) -> a -> m ()
holds p x = do
  ok <- withFrozenCallStack $ eval (p x)
  if ok then
    success
  else
    withFrozenCallStack $ do
      footnoteShow x
      failure
