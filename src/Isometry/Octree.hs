{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Sparse vectors, matrices, and volumes, represented as perfectly balanced binary trees.
module Isometry.Octree
( Shape(..)
, Size
, Finite(..)
, Bin(..)
, Quad(..)
, V(..)
, M(..)
, O(..)
) where

import Data.Proxy
import GHC.TypeLits

-- | The shape of (non-empty) perfectly balanced binary trees.
data Shape
  = L   -- 1
  | B !Shape -- 2 * n

type family Size (b :: Shape) :: Nat where
  Size 'L     = 1
  Size ('B l) = 2 * Size l


class Finite v where
  size :: v a -> Integer


data Bin a = Bin !a !a
  deriving (Foldable, Functor, Traversable)

instance Applicative Bin where
  pure a = Bin a a
  Bin f1 f2 <*> Bin a1 a2 = Bin (f1 a1) (f2 a2)


data Quad a = Quad !a !a !a !a
  deriving (Foldable, Functor, Traversable)

instance Applicative Quad where
  pure a = Quad a a a a
  Quad f1 f2 f3 f4 <*> Quad a1 a2 a3 a4 = Quad (f1 a1) (f2 a2) (f3 a3) (f4 a4)


data Oct a = Oct !a !a !a !a !a !a !a !a
  deriving (Foldable, Functor, Traversable)

instance Applicative Oct where
  pure a = Oct a a a a a a a a
  Oct f1 f2 f3 f4 f5 f6 f7 f8 <*> Oct a1 a2 a3 a4 a5 a6 a7 a8 = Oct (f1 a1) (f2 a2) (f3 a3) (f4 a4) (f5 a5) (f6 a6) (f7 a7) (f8 a8)


-- | Sparse vectors.
data V s a where
  VE :: V s a
  VL :: !a -> V 'L a
  VB :: !(V s a) -> !(V s a) -> V ('B s) a

deriving instance Foldable (V s)
deriving instance Functor (V s)
deriving instance Traversable (V s)

instance Applicative (V 'L) where
  pure = VL

  VE   <*> _ = VE
  VL f <*> a = fmap f a

instance Applicative (V s) => Applicative (V ('B s)) where
  pure a = VB (pure a) (pure a)

  VE       <*> _        = VE
  _        <*> VE       = VE
  VB fl fr <*> VB al ar = VB (fl <*> al) (fr <*> ar)

instance KnownNat (Size s) => Finite (V s) where
  size _ = natVal (Proxy :: Proxy (Size s))



-- fixme: should this be a 2d composition of V?

-- | Sparse square matrices.
data M s a where
  ME :: M s a
  ML :: !a -> M 'L a
  MQ :: !(M s a) -> !(M s a)
     -> !(M s a) -> !(M s a)
     -> M ('B s) a

deriving instance Foldable (M s)
deriving instance Functor (M s)
deriving instance Traversable (M s)

instance Applicative (M 'L) where
  pure = ML

  ME   <*> _ = ME
  ML f <*> a = fmap f a

instance Applicative (M s) => Applicative (M ('B s)) where
  pure a = MQ (pure a) (pure a) (pure a) (pure a)

  ME       <*> _        = ME
  _        <*> ME       = ME
  MQ fx1y1 fx2y1 fx1y2 fx2y2 <*> MQ ax1y1 ax2y1 ax1y2 ax2y2 = MQ (fx1y1 <*> ax1y1) (fx2y1 <*> ax2y1) (fx1y2 <*> ax1y2) (fx2y2 <*> ax2y2)

instance KnownNat (Size s) => Finite (M s) where
  size _ = natVal (Proxy :: Proxy (Size s))


-- fixme: should this be a 3d composition of V?

-- | Sparse cubic volumes.
--
-- Mnemonic: O is for Octree.
data O s a where
  OE :: O s a
  OL :: !a -> O 'L a
  OO :: !(O s a) -> !(O s a)
     -> !(O s a) -> !(O s a)
     -> !(O s a) -> !(O s a)
     -> !(O s a) -> !(O s a)
     -> O ('B s) a

deriving instance Foldable (O s)
deriving instance Functor (O s)
deriving instance Traversable (O s)

instance Applicative (O 'L) where
  pure = OL

  OE   <*> _ = OE
  OL f <*> a = fmap f a

instance Applicative (O s) => Applicative (O ('B s)) where
  pure a = OO (pure a) (pure a) (pure a) (pure a) (pure a) (pure a) (pure a) (pure a)

  OE       <*> _        = OE
  _        <*> OE       = OE
  OO fx1y1z1 fx2y1z1 fx1y2z1 fx2y2z1 fx1y1z2 fx2y1z2 fx1y2z2 fx2y2z2 <*> OO ax1y1z1 ax2y1z1 ax1y2z1 ax2y2z1 ax1y1z2 ax2y1z2 ax1y2z2 ax2y2z2 = OO (fx1y1z1 <*> ax1y1z1) (fx2y1z1 <*> ax2y1z1) (fx1y2z1 <*> ax1y2z1) (fx2y2z1 <*> ax2y2z1) (fx1y1z2 <*> ax1y1z2) (fx2y1z2 <*> ax2y1z2) (fx1y2z2 <*> ax1y2z2) (fx2y2z2 <*> ax2y2z2)

instance KnownNat (Size s) => Finite (O s) where
  size _ = natVal (Proxy :: Proxy (Size s))
