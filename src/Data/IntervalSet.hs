module Data.IntervalSet
( IntervalSet(..)
, empty
, singleton
  -- * Re-exports
, Interval(..)
) where

import Data.Functor.I
import Data.Functor.Interval

data IntervalSet a
  = Empty
  | Branch (IntervalSet a) (Interval I a) (IntervalSet a)

empty :: IntervalSet a
empty = Empty

singleton :: Interval I a -> IntervalSet a
singleton a = Branch empty a empty
