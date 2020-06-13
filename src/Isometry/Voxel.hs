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
, toOX
, toOY
, toOZ
, fromOX
, fromOY
, fromOZ
, toOXY
, toOXZ
, toOYZ
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

toOX :: V x a -> O x 'L 'L a
toOX VE       = OE
toOX (VL a)   = OL a
toOX (VB l r) = OX (toOX l) (toOX r)

toOY :: V y a -> O 'L y 'L a
toOY VE       = OE
toOY (VL a)   = OL a
toOY (VB l r) = OY (toOY l) (toOY r)

toOZ :: V z a -> O 'L 'L z a
toOZ VE       = OE
toOZ (VL a)   = OL a
toOZ (VB l r) = OZ (toOZ l) (toOZ r)

fromOX :: O x 'L 'L a -> V x a
fromOX OE       = VE
fromOX (OL a)   = VL a
fromOX (OX l r) = VB (fromOX l) (fromOX r)

fromOY :: O 'L y 'L a -> V y a
fromOY OE       = VE
fromOY (OL a)   = VL a
fromOY (OY l r) = VB (fromOY l) (fromOY r)

fromOZ :: O 'L 'L z a -> V z a
fromOZ OE       = VE
fromOZ (OL a)   = VL a
fromOZ (OZ l r) = VB (fromOZ l) (fromOZ r)

toOXY :: M x y a -> O x y 'L a
toOXY ME                       = OE
toOXY (ML a)                   = OL a
toOXY (MX l r)                 = OX (toOXY l) (toOXY r)
toOXY (MY l r)                 = OY (toOXY l) (toOXY r)
toOXY (MQ x1y1 x2y1 x1y2 x2y2) = OXY (toOXY x1y1) (toOXY x2y1) (toOXY x1y2) (toOXY x2y2)

toOXZ :: M x z a -> O x 'L z a
toOXZ ME                       = OE
toOXZ (ML a)                   = OL a
toOXZ (MX l r)                 = OX (toOXZ l) (toOXZ r)
toOXZ (MY l r)                 = OZ (toOXZ l) (toOXZ r)
toOXZ (MQ x1y1 x2y1 x1y2 x2y2) = OXZ (toOXZ x1y1) (toOXZ x2y1) (toOXZ x1y2) (toOXZ x2y2)

toOYZ :: M y z a -> O 'L y z a
toOYZ ME                       = OE
toOYZ (ML a)                   = OL a
toOYZ (MX l r)                 = OY (toOYZ l) (toOYZ r)
toOYZ (MY l r)                 = OZ (toOYZ l) (toOYZ r)
toOYZ (MQ x1y1 x2y1 x1y2 x2y2) = OYZ (toOYZ x1y1) (toOYZ x2y1) (toOYZ x1y2) (toOYZ x2y2)


-- FIXME: indicate which sides are present
