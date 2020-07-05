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
      let gi = interval gp
          gs = intervalSet gi
      i <- forAll gi
      s <- insert i <$> forAll gs
      insert i s === s
    ]
  ]
  where
  gp = Gen.int (Range.linear 0 100)

interval :: (MonadGen m, Ord a) => m a -> m (Interval I a)
interval p = mk <$> p <*> p
  where
  mk a b = min a b ... max a b

intervalSet :: (MonadGen m, Ord a) => m (Interval I a) -> m (IntervalSet a)
intervalSet i = foldl' (flip insert) empty <$> Gen.list (Range.linear 0 100) i
