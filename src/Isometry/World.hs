{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
module Isometry.World
( World(..)
, makeWorld
, Distance
) where

import Data.Bin.Tree
import Unit.Length

newtype World s a = World { voxels :: B Oct s a }
  deriving (Foldable, Functor)

makeWorld :: B Oct s a -> World s a
makeWorld voxels = World { voxels }

-- | 1 unit in world space is a semimetre.
type Distance = Semi Metres
