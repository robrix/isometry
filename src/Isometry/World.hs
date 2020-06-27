{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module Isometry.World
( World(..)
, makeWorld
, Distance
) where

import Data.Bin.Tree
import Unit.Length

data World s a = World { size :: {-# UNPACK #-} !Int, voxels :: !(B s Oct a) }
  deriving (Foldable, Functor)

makeWorld :: B s Oct a -> World s a
makeWorld voxels = World { size = length voxels, voxels }

-- | 1 unit in world space is a semimetre.
type Distance = Semi Metres
