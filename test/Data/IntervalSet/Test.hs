{-# LANGUAGE OverloadedStrings #-}
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

tests :: [IO Bool]
tests = map checkParallel
  [ Group "empty" [ ("empty is null", property $ I.null (empty @Int) === True) ]
  , Group "insert"
    [ ("idempotence", property $ do
      i <- forAll gi
      s <- insert i <$> forAll gs
      insert i s === s)
    , ("monotonicity", property $ do
      i <- forAll gi
      s <- forAll gs
      assert . fromMaybe True $ isSubintervalOf <$> bounds s <*> bounds (insert i s)
      assert . fromMaybe True $ (i `isSubintervalOf`) <$> bounds (insert i s))
    ]
  , Group "fromList"
    [ ("inverse", property $ do
      s <- forAll gs
      fromList (toList s) === s)
    ]
  , Group "interval"
    [ ("validity", property $ do
      i <- forAll gi
      assert $ inf i <= sup i)
    , ("coverage", verifiedTermination . withConfidence (10^(6 :: Int)) . property $ do
      i <- forAll gi
      cover 20 "point" (inf i == sup i)
      cover 20 "span" (inf i < sup i))
    ]
  , Group "intervalSet"
    [ ("coverage", property $ do
      s <- forAll gs
      let is = I.toList s
      cover 10 "empty" (I.null s)
      cover 10 "singleton" (length is == 1)
      cover 10 "disjoint" (length is > 1)
      cover 10 "point" (maybe False ((== 0) . size) (bounds s))
      cover 10 "span" (maybe False ((> 0) . size) (bounds s)))
    ]
  ]
  where
  gp = Gen.int (Range.linear 0 100)
  gi = interval gp
  gs = intervalSet gi

interval :: (MonadGen m, Num a) => m a -> m (Interval I a)
interval p = Gen.choice
  [ join (...) <$> p
  , mk <$> p <*> p
  ]
  where
  mk a b = a ... a + b + 1

intervalSet :: (MonadGen m, Ord a) => m (Interval I a) -> m (IntervalSet a)
intervalSet i = Gen.choice
  [ pure empty
  , singleton <$> i
  , fromList <$> Gen.list (Range.linear 0 100) i
  ]
