module Main
( main
) where

import qualified Data.IntervalSet.Test as IntervalSet
import qualified Geometry.Plane.Test as Plane
import qualified Geometry.Transform.Test as Transform
import           Hedgehog.Main

main :: IO ()
main = defaultMain
  [ IntervalSet.tests
  , Plane.tests
  , Transform.tests
  ]
