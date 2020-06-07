module Isometry.Voxel
( B(..)
) where

-- | The shape of binary trees.
data B
  = Z
  | L
  | B B B
