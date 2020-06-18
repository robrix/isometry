{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Sparse vectors, matrices, and volumes, represented as perfectly balanced binary trees.
module Isometry.Octree
( Shape(..)
, Size
, B(..)
, size
, capacity
, Bin(..)
, Quad(..)
, Oct(..)
) where

import           Data.Proxy
import           Data.Vector ((!))
import           GHC.TypeLits
import qualified Linear.V as Linear

-- | The shape of (non-empty) perfectly balanced binary trees.
data Shape
  = S1         -- 1
  | S2x !Shape -- 2 * n

type family Size (b :: Shape) :: Nat where
  Size 'S1      = 1
  Size ('S2x l) = 2 * Size l


data B s f a where
  E :: B s f a
  L :: !a -> B 'S1 f a
  B :: !(f (B s f a)) -> B ('S2x s) f a

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

size :: forall s f a . KnownNat (Size s) => B s f a -> Integer
size _ = natVal (Proxy @(Size s))

capacity :: forall s f a . (KnownNat (Linear.Size f), KnownNat (Size s)) => B s f a -> Integer
capacity b = natVal (Proxy @(Linear.Size f)) * size b


data Bin a = Bin
  { l :: !a
  , r :: !a
  }
  deriving (Foldable, Functor, Traversable)

instance Applicative Bin where
  pure a = Bin a a
  Bin f1 f2 <*> Bin a1 a2 = Bin (f1 a1) (f2 a2)

instance Linear.Finite Bin where
  type Size Bin = 2

  fromV (Linear.V v) = Bin (v ! 0) (v ! 1)

instance Semigroup a => Semigroup (Bin a) where
  Bin l1 r1 <> Bin l2 r2 = Bin (l1 <> l2) (r1 <> r2)

instance Monoid a => Monoid (Bin a) where
  mempty = Bin mempty mempty


data Quad a = Quad
  { bl :: !a
  , br :: !a
  , tl :: !a
  , tr :: !a
  }
  deriving (Foldable, Functor, Traversable)

instance Applicative Quad where
  pure a = Quad a a a a
  Quad f1 f2 f3 f4 <*> Quad a1 a2 a3 a4 = Quad (f1 a1) (f2 a2) (f3 a3) (f4 a4)

instance Linear.Finite Quad where
  type Size Quad = 4

  fromV (Linear.V v) = Quad (v ! 0) (v ! 1) (v ! 2) (v ! 3)

instance Semigroup a => Semigroup (Quad a) where
  Quad bl1 br1 tl1 tr1 <> Quad bl2 br2 tl2 tr2 = Quad (bl1 <> bl2) (br1 <> br2) (tl1 <> tl2) (tr1 <> tr2)

instance Monoid a => Monoid (Quad a) where
  mempty = Quad mempty mempty mempty mempty


data Oct a = Oct
  { bln :: !a
  , brn :: !a
  , tln :: !a
  , trn :: !a
  , blf :: !a
  , brf :: !a
  , tlf :: !a
  , trf :: !a
  }
  deriving (Foldable, Functor, Traversable)

instance Applicative Oct where
  pure a = Oct a a a a a a a a
  Oct f1 f2 f3 f4 f5 f6 f7 f8 <*> Oct a1 a2 a3 a4 a5 a6 a7 a8 = Oct (f1 a1) (f2 a2) (f3 a3) (f4 a4) (f5 a5) (f6 a6) (f7 a7) (f8 a8)

instance Linear.Finite Oct where
  type Size Oct = 8

  fromV (Linear.V v) = Oct (v ! 0) (v ! 1) (v ! 2) (v ! 3) (v ! 4) (v ! 5) (v ! 6) (v ! 7)

instance Semigroup a => Semigroup (Oct a) where
  Oct bln1 brn1 tln1 trn1 blf1 brf1 tlf1 trf1 <> Oct bln2 brn2 tln2 trn2 blf2 brf2 tlf2 trf2 = Oct (bln1 <> bln2) (brn1 <> brn2) (tln1 <> tln2) (trn1 <> trn2) (blf1 <> blf2) (brf1 <> brf2) (tlf1 <> tlf2) (trf1 <> trf2)

instance Monoid a => Monoid (Oct a) where
  mempty = Oct mempty mempty mempty mempty mempty mempty mempty mempty
