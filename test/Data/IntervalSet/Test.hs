{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Data.IntervalSet.Test
( tests
) where

import           Data.Functor.I
import           Data.Functor.Interval
import           Data.Functor.Interval.Test (interval)
import           Data.IntervalSet as I
import           Data.Maybe (fromMaybe)
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

tests :: [IO Bool]
tests = map checkParallel
  [ Group "empty" [ ("empty is null", withTests 1 . property $ I.null (empty @I @Int) === True) ]
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
  , Group "delete"
    [ ("inverse", property $ do
      s <- forAll gs
      i <- forAll gi
      insert i (delete i s) === insert i s)
    , ("annihilation", property $ do
      s <- forAll gs
      maybe id delete (bounds s) s === empty)
    ]
  , Group "fromList"
    [ ("inverse", property $ do
      s <- forAll gs
      fromList (toList s) === s)
    ]
  , Group "splitAround"
    [ ("left is less than infimum", property $ do
      s <- forAll gs
      i <- forAll gi
      let (l, _, _) = splitAround i s
      all ((< inf i) . sup) (toList l) === True)
    , ("right is greater than supremum", property $ do
      s <- forAll gs
      i <- forAll gi
      let (_, _, r) = splitAround i s
      all ((sup i <) . inf) (toList r) === True)
    , ("centre is overlapping", property $ do
      s <- forAll gs
      i <- forAll gi
      let (_, c, _) = splitAround i s
      all (intersects i) (toList c) === True)
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

intervalSet :: (MonadGen m, Ord a) => m (Interval I a) -> m (IntervalSet I a)
intervalSet i = Gen.choice
  [ pure empty
  , singleton <$> i
  , fromList <$> Gen.list (Range.linear 0 100) i
  ]
