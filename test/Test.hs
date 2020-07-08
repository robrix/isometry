module Main
( main
) where

import qualified Data.IntervalSet.Test as IntervalSet
import           Hedgehog.Main

main :: IO ()
main = defaultMain IntervalSet.tests
