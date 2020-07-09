module Main
( main
) where

import qualified Data.Functor.Interval.Test as Interval
import qualified Data.IntervalSet.Test as IntervalSet
import           Hedgehog.Main

main :: IO ()
main = defaultMain $ concat
  [ Interval.tests
  , IntervalSet.tests
  ]
