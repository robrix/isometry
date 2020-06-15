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
, B(..)
, Bin(..)
, Quad(..)
, Oct(..)
) where

import Data.Proxy
import GHC.TypeLits

-- | The shape of (non-empty) perfectly balanced binary trees.
data Shape
  = S1         -- 1
  | S2x !Shape -- 2 * n

type family Size (b :: Shape) :: Nat where
  Size 'S1      = 1
  Size ('S2x l) = 2 * Size l


class Finite v where
  size :: v a -> Integer


data B s f a where
  E :: B s f a
  L :: a -> B 'S1 f a
  B :: f (B s f a) -> B ('S2x s) f a

deriving instance Foldable f => Foldable (B s f)
deriving instance Functor f => Functor (B s f)
deriving instance Traversable f => Traversable (B s f)

instance Functor f => Applicative (B 'S1 f) where
  pure = L

  E   <*> _ = E
  L f <*> a = fmap f a

instance (Applicative f, Applicative (B s f)) => Applicative (B ('S2x s) f) where
  pure a = B (pure (pure a))

  E   <*> _   = E
  _   <*> E   = E
  B f <*> B a = B ((<*>) <$> f <*> a)

instance KnownNat (Size s) => Finite (B s f) where
  size _ = natVal (Proxy :: Proxy (Size s))


data Bin a = Bin
  { x1 :: !a
  , x2 :: !a
  }
  deriving (Foldable, Functor, Traversable)

instance Applicative Bin where
  pure a = Bin a a
  Bin f1 f2 <*> Bin a1 a2 = Bin (f1 a1) (f2 a2)


data Quad a = Quad
  { x1y1 :: !a
  , x2y1 :: !a
  , x1y2 :: !a
  , x2y2 :: !a
  }
  deriving (Foldable, Functor, Traversable)

instance Applicative Quad where
  pure a = Quad a a a a
  Quad f1 f2 f3 f4 <*> Quad a1 a2 a3 a4 = Quad (f1 a1) (f2 a2) (f3 a3) (f4 a4)


data Oct a = Oct
  { x1y1z1 :: !a
  , x2y1z1 :: !a
  , x1y2z1 :: !a
  , x2y2z1 :: !a
  , x1y1z2 :: !a
  , x2y1z2 :: !a
  , x1y2z2 :: !a
  , x2y2z2 :: !a
  }
  deriving (Foldable, Functor, Traversable)

instance Applicative Oct where
  pure a = Oct a a a a a a a a
  Oct f1 f2 f3 f4 f5 f6 f7 f8 <*> Oct a1 a2 a3 a4 a5 a6 a7 a8 = Oct (f1 a1) (f2 a2) (f3 a3) (f4 a4) (f5 a5) (f6 a6) (f7 a7) (f8 a8)
