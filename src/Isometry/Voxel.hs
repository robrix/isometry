{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
module Isometry.Voxel
( B(..)
, V(..)
) where

-- | The shape of binary trees.
data B
  = Z
  | L
  | B B B

-- | Sparse vectors.
data V s a where
  VZ :: V s a
  VL :: V 'L a
  VB :: V s1 a -> V s2 a -> V ('B s1 s2) a
