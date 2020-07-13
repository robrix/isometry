{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
module Isometry.Voxel
( Voxel(..)
, Dimension(..)
, Polarity(..)
) where

import           Data.Ix
import           Foreign.Ptr
import           Foreign.Storable
import           GHC.Generics (Generic)
import           Isometry.World
import           Linear.V3
import qualified UI.Colour as UI

-- FIXME: indicate which sides are present

data Voxel = Voxel
  { origin :: {-# UNPACK #-} !(V3 (Distance Float))
  , colour :: {-# UNPACK #-} !(UI.Colour Float)
  }
  deriving (Generic)

instance UI.HasColour Voxel

instance Storable Voxel where
 sizeOf _ = sizeOf @(V3 (Distance Float)) undefined + sizeOf @(UI.Colour Float) undefined
 alignment _ = alignment @(V3 (Distance Float)) undefined
 peek ptr = Voxel <$> peek (castPtr ptr) <*> peek (castPtr ptr `plusPtr` sizeOf @(V3 (Distance Float)) undefined)
 poke ptr (Voxel o c) = poke (castPtr ptr) o *> poke (castPtr ptr `plusPtr` sizeOf @(V3 (Distance Float)) undefined) c

data Dimension
  = X
  | Y
  | Z
  deriving (Enum, Eq, Ix, Ord, Show)

data Polarity
  = Neg
  | Pos
  deriving (Enum, Eq, Ix, Ord, Show)
