{-# LANGUAGE TypeApplications #-}
module Data.IntervalSet.Test
( tests
) where

import Data.IntervalSet as I
import Hedgehog
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "IntervalSet"
  [ testProperty "empty is null" . property $ I.null (empty @Int) === True
  ]
