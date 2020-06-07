{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
module Isometry.Voxel
( B(..)
, V(..)
, M(..)
) where

-- | The shape of binary trees.
data B
  = E
  | L
  | B B B

-- | Sparse vectors.
data V s a where
  VE :: V s a
  VL :: a -> V 'L a
  VB :: V s1 a -> V s2 a -> V ('B s1 s2) a

-- | Sparse matrices.
data M x y a where
  ME :: M x y a
  ML :: a -> M 'L 'L a
  MX :: M x1 'L a -> M x2 'L a -> M ('B x1 x2) 'L a
  MY :: M 'L y1 a -> M 'L y2 a -> M 'L ('B y1 y2) a
  MQ :: M x1 y1 a -> M x2 y1 a
     -> M x1 y2 a -> M x2 y2 a
     -> M ('B x1 x2) ('B y1 y2) a
