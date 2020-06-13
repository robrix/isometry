{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
module Isometry.Voxel
( B(..)
, V(..)
, M(..)
, toMX
, toMY
, fromMX
, fromMY
, O(..)
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

deriving instance Foldable (V s)


-- | Sparse matrices.
data M x y a where
  ME :: M x y a
  ML :: a -> M 'L 'L a
  -- FIXME: should MX & MY hold a vector instead?
  MX :: M x1 'L a -> M x2 'L a -> M ('B x1 x2) 'L a
  MY :: M 'L y1 a -> M 'L y2 a -> M 'L ('B y1 y2) a
  MQ :: M x1 y1 a -> M x2 y1 a
     -> M x1 y2 a -> M x2 y2 a
     -> M ('B x1 x2) ('B y1 y2) a

deriving instance Foldable (M x y)

toMX :: V x a -> M x 'L a
toMX VE       = ME
toMX (VL a)   = ML a
toMX (VB l r) = MX (toMX l) (toMX r)

toMY :: V y a -> M 'L y a
toMY VE       = ME
toMY (VL a)   = ML a
toMY (VB l r) = MY (toMY l) (toMY r)

fromMX :: M x 'L a -> V x a
fromMX ME       = VE
fromMX (ML a)   = VL a
fromMX (MX l r) = VB (fromMX l) (fromMX r)

fromMY :: M 'L y a -> V y a
fromMY ME       = VE
fromMY (ML a)   = VL a
fromMY (MY l r) = VB (fromMY l) (fromMY r)


-- | Sparse volumes.
--
-- Mnemonic: O is for Octree.
data O x y z a where
  OE :: O x y z a
  OL :: a -> O 'L 'L 'L a
  -- FIXME: should OX, OY, & OZ hold a vector instead?
  OX :: O x1 'L 'L a -> O x2 'L 'L a -> O ('B x1 x2) 'L 'L a
  OY :: O 'L y1 'L a -> O 'L y2 'L a -> O 'L ('B y1 y2) 'L a
  OZ :: O 'L 'L z1 a -> O 'L 'L z2 a -> O 'L 'L ('B z1 z2) a
  -- FIXME: should OXY, OYZ, & OXZ hold a matrix instead?
  OXY :: O x1 y1 'L a -> O x2 y1 'L a
      -> O x1 y2 'L a -> O x2 y2 'L a
      -> O ('B x1 x2) ('B y1 y2) 'L a
  OYZ :: O 'L y1 z1 a -> O 'L y2 z1 a
      -> O 'L y1 z2 a -> O 'L y2 z2 a
      -> O 'L ('B y1 y2) ('B z1 z2) a
  OXZ :: O x1 'L z1 a -> O x2 'L z1 a
      -> O x1 'L z2 a -> O x2 'L z2 a
      -> O ('B x1 x2) 'L ('B z1 z2) a
  OO :: O x1 y1 z1 a -> O x2 y1 z1 a
     -> O x1 y2 z1 a -> O x2 y2 z1 a
     -> O x1 y1 z2 a -> O x2 y1 z2 a
     -> O x1 y2 z2 a -> O x2 y2 z2 a
     -> O ('B x1 x2) ('B y1 y2) ('B z1 z2) a

deriving instance Foldable (O x y z)


-- FIXME: indicate which sides are present
