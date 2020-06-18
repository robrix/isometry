module Isometry.Voxel
( Voxel(..)
) where

import qualified UI.Colour as UI

-- FIXME: indicate which sides are present

newtype Voxel = Voxel { colour :: UI.Colour Float }
