module Isometry.World
( World
, Octree(..)
) where

import Isometry.Octree

data World

newtype Octree s a = Octree { voxels :: B s Oct a }
