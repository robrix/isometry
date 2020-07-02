module Data.IntervalSet
( IntervalSet(..)
  -- * Re-exports
, Interval(..)
) where

import Data.Functor.I
import Data.Functor.Interval

data IntervalSet a
  = Empty
  | Branch (IntervalSet a) (Interval I a) (IntervalSet a)
