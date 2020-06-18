{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE QuantifiedConstraints #-}
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
, Bit(..)
, toIndex
, Index(..)
, toFraction
, B(..)
, size
, capacity
, Bin(..)
, Quad(..)
, Oct(..)
  -- * Tree generation
, Tetra(..)
, UnfoldableWithIndex(..)
, iunfold
) where

import           Control.Carrier.State.Church
import           Control.Lens.Indexed
import           Data.Proxy
import           Data.Ratio ((%))
import           Data.Vector ((!))
import           GHC.TypeLits
import qualified Linear.V as Linear
import           Linear.V1
import           Linear.V2
import           Linear.V3

-- | The shape of (non-empty) perfectly balanced binary trees.
--
-- This represents shapes of size 2ⁿ.
data Shape
  = S1         -- 1
  | S2x !Shape -- 2 * n

type family Size (b :: Shape) :: Nat where
  Size 'S1      = 1
  Size ('S2x l) = 2 * Size l


data Bit
  = I0
  | I1
  deriving (Enum, Eq, Ord, Show)

toIndex :: Bit -> Index i -> Index ('S2x i)
toIndex I0 = IL
toIndex I1 = IR

data Index i where
  II :: Index 'S1
  IL :: Index s -> Index ('S2x s)
  IR :: Index s -> Index ('S2x s)

toFraction :: Index i -> Rational
toFraction = uncurry (%) . go
  where
  go :: Index i -> (Integer, Integer)
  go II     = (0, 1)
  go (IL i) = let (n, d) = go i in (n, d * 2)
  go (IR i) = let (n, d) = go i in (n + d, d * 2)


data B s f a where
  E :: B s f a
  L :: !a -> B 'S1 f a
  B :: !(f (B s f a)) -> B ('S2x s) f a

deriving instance Foldable f => Foldable (B s f)
deriving instance Functor f => Functor (B s f)
deriving instance Traversable f => Traversable (B s f)

instance (FoldableWithIndex (v Bit) f, Applicative v) => FoldableWithIndex (v (Index s)) (B s f) where
  ifoldMap _ E     = mempty
  ifoldMap f (L a) = f (pure II) a
  ifoldMap f (B b) = ifoldMap (\ i -> ifoldMap (\ j -> f (toIndex <$> i <*> j))) b

instance (FunctorWithIndex (v Bit) f, Applicative v) => FunctorWithIndex (v (Index s)) (B s f) where
  imap _ E     = E
  imap f (L a) = L (f (pure II) a)
  imap f (B b) = B (imap (\ i -> imap (\ j -> f (toIndex <$> i <*> j))) b)

instance (FoldableWithIndex (v Bit) f, FunctorWithIndex (v Bit) f, TraversableWithIndex (v Bit) f, Applicative v) => TraversableWithIndex (v (Index s)) (B s f) where
  itraverse _ E     = pure E
  itraverse f (L a) = L <$> f (pure II) a
  itraverse f (B b) = B <$> itraverse (\ i -> itraverse (\ j -> f (toIndex <$> i <*> j))) b

-- | Note that this instance can only express dense unfoldings.
instance (UnfoldableWithIndex (v Bit) f, Applicative v) => UnfoldableWithIndex (v (Index 'S1)) (B 'S1 f) where
  iunfoldA f = L <$> f (pure II)

-- | Note that this instance can only express dense unfoldings.
instance (UnfoldableWithIndex (v Bit) f, Applicative v, UnfoldableWithIndex (v (Index s)) (B s f)) => UnfoldableWithIndex (v (Index ('S2x s))) (B ('S2x s) f) where
  iunfoldA f = B <$> iunfoldA (\ i -> iunfoldA (\ j -> f (toIndex <$> i <*> j)))

instance Functor f => Applicative (B 'S1 f) where
  pure = L

  E   <*> _ = E
  L f <*> a = fmap f a

instance (Applicative f, Applicative (B s f)) => Applicative (B ('S2x s) f) where
  pure a = B (pure (pure a))

  E   <*> _   = E
  _   <*> E   = E
  B f <*> B a = B ((<*>) <$> f <*> a)

instance (Semigroup a, forall x . Semigroup x => Semigroup (f x)) => Semigroup (B s f a) where
  E   <> b   = b
  a   <> E   = a
  L a <> L b = L (a <> b)
  B a <> B b = B (a <> b)

instance (Semigroup a, forall x . Semigroup x => Semigroup (f x)) => Monoid (B s f a) where
  mempty = E

size :: forall s f a . KnownNat (Size s) => B s f a -> Integer
size _ = natVal (Proxy @(Size s))

capacity :: forall s f a . (KnownNat (Linear.Size f), KnownNat (Size s)) => B s f a -> Integer
capacity b = natVal (Proxy @(Linear.Size f)) * size b


-- | Binary nodes.
--
-- Mnemonic for fields: left/right.
data Bin a = Bin
  { l :: !a
  , r :: !a
  }
  deriving (Foldable, Functor, Traversable)

instance FoldableWithIndex (V1 Bit) Bin
instance FunctorWithIndex (V1 Bit) Bin
instance TraversableWithIndex (V1 Bit) Bin where
  itraverse f (Bin l r) = Bin <$> f (V1 I0) l <*> f (V1 I1) r

instance UnfoldableWithIndex (V1 Bit) Bin where
  iunfoldA f = Bin
    <$> f (V1 I0)
    <*> f (V1 I1)

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


-- | Quaternary nodes.
--
-- Mnemonic for fields: bottom/top, left/right.
data Quad a = Quad
  { bl :: !a
  , br :: !a
  , tl :: !a
  , tr :: !a
  }
  deriving (Foldable, Functor, Traversable)

instance FoldableWithIndex (V2 Bit) Quad
instance FunctorWithIndex (V2 Bit) Quad
instance TraversableWithIndex (V2 Bit) Quad where
  itraverse f (Quad bl br tl tr) = Quad <$> f (V2 I0 I0) bl <*> f (V2 I1 I0) br <*> f (V2 I0 I1) tl <*> f (V2 I1 I1) tr

instance UnfoldableWithIndex (V2 Bit) Quad where
  iunfoldA f = Quad
    <$> f (V2 I0 I0)
    <*> f (V2 I1 I0)
    <*> f (V2 I0 I1)
    <*> f (V2 I1 I1)

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


-- | Octonary nodes.
--
-- Mnemonic for fields: bottom/top, left/right, near/far.
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

instance FoldableWithIndex (V3 Bit) Oct
instance FunctorWithIndex (V3 Bit) Oct
instance TraversableWithIndex (V3 Bit) Oct where
  itraverse f (Oct bln brn tln trn blf brf tlf trf) = Oct
    <$> f (V3 I0 I0 I0) bln <*> f (V3 I1 I0 I0) brn
    <*> f (V3 I0 I1 I0) tln <*> f (V3 I1 I1 I0) trn
    <*> f (V3 I0 I0 I1) blf <*> f (V3 I1 I0 I1) brf
    <*> f (V3 I0 I1 I1) tlf <*> f (V3 I1 I1 I1) trf

instance UnfoldableWithIndex (V3 Bit) Oct where
  iunfoldA f = Oct
    <$> f (V3 I0 I0 I0)
    <*> f (V3 I1 I0 I0)
    <*> f (V3 I0 I1 I0)
    <*> f (V3 I1 I1 I0)
    <*> f (V3 I0 I0 I1)
    <*> f (V3 I1 I0 I1)
    <*> f (V3 I0 I1 I1)
    <*> f (V3 I1 I1 I1)

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


-- | Tree generation.
class Tetra s where
  tetra :: B s Oct ()

instance Tetra 'S1 where
  tetra = L ()

instance Tetra s => Tetra ('S2x s) where
  tetra = B $ Oct
    E     tetra
    tetra E
    tetra E
    E     tetra


-- | Unfolding of finite structures with an index.
class UnfoldableWithIndex i f | f -> i where
  iunfoldA :: Applicative m => (i -> m b) -> m (f b)

iunfold :: UnfoldableWithIndex i f => (i -> s -> (s, b)) -> s -> f b
iunfold f a = run . evalState a . iunfoldA $ state . f