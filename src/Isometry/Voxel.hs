{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
module Isometry.Voxel
( Voxel(..)
, Side(..)
) where

import           Control.Lens.Iso (from)
import           Data.Generics.Product.Fields
import           Data.Ix
import           Data.Word
import           GHC.Generics (Generic)
import qualified UI.Colour as UI

-- FIXME: indicate which sides are present

newtype Voxel = Voxel { colour :: Word32 }
  deriving (Generic)

instance UI.HasColour Voxel where
  colour_ = field @"colour".from UI.packed

data Side
  = L
  | R
  | B
  | T
  | F
  | N
  deriving (Enum, Eq, Ix, Ord, Show)
