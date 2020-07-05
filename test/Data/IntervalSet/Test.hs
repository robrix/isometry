{-# LANGUAGE TypeApplications #-}
module Data.IntervalSet.Test
( tests
) where

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
      i <- forAll (interval (Gen.int (Range.linear 0 100)))
      let s = insert i empty
      insert i s === s
    ]
  ]

interval :: MonadGen m => m a -> m (Interval I a)
interval p = (...) <$> p <*> p
