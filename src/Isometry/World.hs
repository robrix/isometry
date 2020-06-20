{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Isometry.World
( World
, Octree(..)
, Distance
) where

import Data.Bin.Tree
import Unit.Length

data World

newtype Octree s a = Octree { voxels :: B s Oct a }
  deriving (Foldable, Functor)

-- | 1 unit in world space is a semimetre.
type Distance = Semi Metres
