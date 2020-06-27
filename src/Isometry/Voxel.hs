{-# LANGUAGE DeriveGeneric #-}
module Isometry.Voxel
( Voxel(..)
, Side(..)
) where

import           Data.Ix
import           GHC.Generics (Generic)
import qualified UI.Colour as UI

-- FIXME: indicate which sides are present

newtype Voxel = Voxel { colour :: UI.Colour Float }
  deriving (Generic)

instance UI.HasColour Voxel

data Side
  = L
  | R
  | B
  | T
  | F
  | N
  deriving (Enum, Eq, Ix, Ord, Show)
