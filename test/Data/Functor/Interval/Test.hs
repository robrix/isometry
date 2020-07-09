module Data.Functor.Interval.Test
( tests
) where

import Hedgehog

tests :: [IO Bool]
tests = map checkParallel
  []
