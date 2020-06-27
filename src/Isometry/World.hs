{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Isometry.World
( World(..)
, Distance
) where

import Data.Bin.Tree
import Unit.Length

newtype World s a = World { voxels :: B s Oct a }
  deriving (Foldable, Functor)

-- | 1 unit in world space is a semimetre.
type Distance = Semi Metres
