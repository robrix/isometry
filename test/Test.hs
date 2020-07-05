module Main
( main
) where

import qualified Data.IntervalSet.Test as IntervalSet
import           Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "unit tests"
  [ IntervalSet.tests
  ]
