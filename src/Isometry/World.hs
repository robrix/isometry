module Isometry.World
( World
, Octree(..)
, Distance
) where

import Isometry.Octree
import Unit.Length

data World

newtype Octree s a = Octree { voxels :: B s Oct a }

-- | 1 unit in world space is a semimetre.
type Distance = Semi Metres
