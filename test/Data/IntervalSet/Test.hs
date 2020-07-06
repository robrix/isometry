{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Data.IntervalSet.Test
( tests
) where

import Data.Foldable (foldl')
import Data.Functor.I
import Data.Functor.Interval
import Data.IntervalSet as I
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "IntervalSet"
  [ testProperty "empty is null" . property $ I.null (empty @Int) === True
  , testGroup "insert"
    [ testProperty "idempotence" . property $ do
      let gs = intervalSet gi
      i <- forAll gi
      s <- insert i <$> forAll gs
      insert i s === s
    ]
  , testGroup "larger"
    [ testProperty "monotonicity" . property $ do
      i1 <- forAll gi
      i2 <- forAll gi
      i3 <- forAll gi
      if larger i1 (Just i2) && larger i1 (Just i3) then do
        label "i1 > i2 ∧ i1 > i3"
        larger i1 (Just (i2 <> i3)) === True
      else
        label "i1 < i2 ∨ i1 < i3"
    ]
  , testGroup "smaller"
    [ testProperty "monotonicity" . property $ do
      i1 <- forAll gi
      i2 <- forAll gi
      i3 <- forAll gi
      if smaller i1 (Just i2) && smaller i1 (Just i3) then do
        label "i1 < i2 ∧ i1 < i3"
        smaller i1 (Just (i2 <> i3)) === True
      else
        label "i1 > i2 ∨ i1 > i3"
    ]
  ]
  where
  gp = Gen.int (Range.linear 0 100)
  gi = interval gp

interval :: (MonadGen m, Ord a) => m a -> m (Interval I a)
interval p = mk <$> p <*> p
  where
  mk a b = min a b ... max a b

intervalSet :: (MonadGen m, Ord a) => m (Interval I a) -> m (IntervalSet a)
intervalSet i = foldl' (flip insert) empty <$> Gen.list (Range.linear 0 100) i
