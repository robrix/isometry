module Isometry.Voxel
( Voxel(..)
, Side(..)
) where

import           Data.Ix
import qualified UI.Colour as UI

-- FIXME: indicate which sides are present

newtype Voxel = Voxel { colour :: UI.Colour Float }

data Side
  = L
  | R
  | B
  | T
  | F
  | N
  deriving (Enum, Eq, Ix, Ord, Show)
