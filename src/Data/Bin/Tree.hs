{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Sparse vectors, matrices, and volumes, represented as perfectly balanced binary trees.
module Data.Bin.Tree
( B(..)
, isE
, b
, size
, capacity
, Bintree
, Bin(..)
, bin
, bin_
, l_
, r_
, Quadtree
, Quad(..)
, quad
, quad_
, bl_
, br_
, tl_
, tr_
, Octree
, Oct(..)
, oct
, oct_
, bln_
, brn_
, tln_
, trn_
, blf_
, brf_
, tlf_
, trf_
  -- * Tree generation
, tetra
, UnfoldableWithIndex(..)
, SparseUnfoldableWithIndex(..)
, iunfoldr
-- * Indexing
, BinaryIndexed(..)
, Indexed(..)
, SparseIndexed(..)
, MutableIndexed(..)
, deinterleaveWith
) where

import           Control.Applicative (liftA2, liftA3)
import           Control.Carrier.State.Church
import           Control.Lens (Iso', Lens', iso, set, (^.))
import           Control.Lens.Indexed hiding (Indexed(..), indices)
import           Data.Bin.Bit
import           Data.Bin.Index
import           Data.Bin.Shape
import           Data.Bits
import           Data.Coerce
import           Data.Foldable (foldl')
import           Data.Functor.C
import           Data.Monoid (Sum(..))
import           Data.Proxy
import qualified Data.Vector as V
import           GHC.Generics (Generic, Generic1)
import           GHC.TypeLits
import qualified Linear.V as Linear
import           Linear.V1
import           Linear.V2
import           Linear.V3
import           Linear.Vector (E(el))

data B f s a where
  E :: B f s a
  L :: !a -> B f 'S1 a
  B :: {-# UNPACK #-} !Int -> !(f (B f s a)) -> B f ('S2x s) a

instance Foldable f => Foldable (B f s) where
  foldMap (f :: a -> m) = go
    where
    go :: B f s' a -> m
    go = \case
      E     -> mempty
      L a   -> f a
      B _ b -> foldMap go b

  length = \case
    E     -> 0
    L _   -> 1
    B l _ -> l

deriving instance Functor f => Functor (B f s)
deriving instance Traversable f => Traversable (B f s)

instance (FoldableWithIndex (v Bit) f, Applicative v) => FoldableWithIndex (v (Index s)) (B f s) where
  ifoldMap _ E       = mempty
  ifoldMap f (L   a) = f (pure IL) a
  ifoldMap f (B _ b) = ifoldMap (\ i -> ifoldMap (\ j -> f (IB <$> i <*> j))) b

instance (FunctorWithIndex (v Bit) f, Applicative v) => FunctorWithIndex (v (Index s)) (B f s) where
  imap _ E       = E
  imap f (L   a) = L (f (pure IL) a)
  imap f (B l b) = B l (imap (\ i -> imap (\ j -> f (IB <$> i <*> j))) b)

instance (FoldableWithIndex (v Bit) f, FunctorWithIndex (v Bit) f, TraversableWithIndex (v Bit) f, Applicative v) => TraversableWithIndex (v (Index s)) (B f s) where
  itraverse _ E       = pure E
  itraverse f (L   a) = L <$> f (pure IL) a
  itraverse f (B l b) = B l <$> itraverse (\ i -> itraverse (\ j -> f (IB <$> i <*> j))) b

-- | Note that this instance can only express dense unfoldings.
instance (UnfoldableWithIndex (v Bit) f, Applicative v) => UnfoldableWithIndex (v (Index 'S1)) (B f 'S1) where
  iunfoldA f = L <$> f (pure IL)

-- | Note that this instance can only express dense unfoldings.
instance (UnfoldableWithIndex (v Bit) f, Applicative v, UnfoldableWithIndex (v (Index s)) (B f s), Foldable f) => UnfoldableWithIndex (v (Index ('S2x s))) (B f ('S2x s)) where
  iunfoldA f = makeB <$> iunfoldA (\ i -> iunfoldA (\ j -> f (IB <$> i <*> j)))

instance (Applicative v, UnfoldableWithIndex (v Bit) f) => SparseUnfoldableWithIndex v (Index 'S1) (B f 'S1) where
  iunfoldSparseM _ leaf = L <$> leaf (pure IL)

instance (Applicative v, UnfoldableWithIndex (v Bit) f, SparseUnfoldableWithIndex v (Index s) (B f s), Foldable f) => SparseUnfoldableWithIndex v (Index ('S2x s)) (B f ('S2x s)) where
  iunfoldSparseM branch leaf = b <$> iunfoldA (\ i -> branch i >>= \ b -> if b then iunfoldSparseM branch (leaf . (IB <$> i <*>)) else pure E)

instance (Indexed (v Bit) f, Functor v) => SparseIndexed (v (Index s)) (B f s) where
  E     !? _ = Nothing
  L   a !? _ = Just a
  B _ b !? v = b ! fmap (fst . fromIndex) v !? fmap (snd . fromIndex) v

instance MutableIndexed (v Bit) f => MutableIndexed (v (Index 'S1)) (B f 'S1) where
  insert _ a _ = L a

instance (MutableIndexed (v Bit) f, Applicative f, Foldable f, Functor v, MutableIndexed (v (Index s)) (B f s), Indexed (v Bit) f) => MutableIndexed (v (Index ('S2x s))) (B f ('S2x s)) where
  insert i a = makeB . uncurry (insert ihead . insert itail a) . \case
    E     -> (E, pure E)
    B _ f -> (f ! ihead, f)
    where
    ihead = fst . fromIndex <$> i
    itail = snd . fromIndex <$> i

instance Functor f => Applicative (B f 'S1) where
  pure = L

  E   <*> _ = E
  L f <*> a = fmap f a

instance (Applicative f, Applicative (B f s), Foldable f) => Applicative (B f ('S2x s)) where
  -- FIXME: this could probably just use the capacity instead
  pure a = makeB (pure (pure a))

  E     <*> _     = E
  _     <*> E     = E
  B _ f <*> B _ a = makeB ((<*>) <$> f <*> a)

instance (Semigroup a, forall x . Semigroup x => Semigroup (f x), Foldable f) => Semigroup (B f s a) where
  E     <> b     = b
  a     <> E     = a
  L   a <> L   b = L (a <> b)
  B _ a <> B _ b = makeB (a <> b)

instance (Semigroup a, forall x . Semigroup x => Semigroup (f x), Foldable f) => Monoid (B f s a) where
  mempty = E

isE :: B s f a -> Bool
isE E = True
isE _ = False

b :: Foldable f => f (B f s a) -> B f ('S2x s) a
b f | all isE f = E
    | otherwise = makeB f

makeB :: Foldable f => f (B f s a) ->  B f ('S2x s) a
makeB a = B (getSum (foldMap (Sum . length) a)) a

capacity :: forall s f a . (KnownNat (Linear.Size f), KnownNat (Size s)) => B f s a -> Integer
capacity b = size b ^ (round (logBase @Float 2 (fromIntegral (natVal (Proxy @(Linear.Size f))))) :: Int)


type Bintree = B Bin

-- | Binary nodes.
--
-- Mnemonic for fields: left/right.
newtype Bin a = Bin { getBin :: V2 a }
  deriving (Applicative, Foldable, Functor, Generic, Generic1, Monoid, Semigroup, Traversable)

instance FoldableWithIndex (V1 Bit) Bin
instance FunctorWithIndex (V1 Bit) Bin
instance TraversableWithIndex (V1 Bit) Bin where
  itraverse f (Bin b) = Bin <$> itraverse (\ ix -> f (indices^.el ix)) b
    where
    indices = head (deinterleaveWith V2 (V1 <$> [I0, I1]))

instance UnfoldableWithIndex (V1 Bit) Bin where
  iunfoldA f = bin
    <$> f (V1 I0)
    <*> f (V1 I1)

instance Indexed (V1 Bit) Bin where
  b ! i = case i of
    V1 I0 -> b^.l_
    V1 I1 -> b^.r_

instance MutableIndexed (V1 Bit) Bin where
  insert (V1 I0) = set l_
  insert (V1 I1) = set r_

instance Linear.Finite Bin where
  type Size Bin = 2

  fromV (Linear.V v) = Bin (head (deinterleaveWith V2 (map (v V.!) [0..1])))

instance R1 Bin where
  _x = _xy._x

instance R2 Bin where
  _xy = bin_

bin :: forall a . a -> a -> Bin a
bin = coerce (V2 :: a -> a -> V2 a)

bin_ :: Iso' (Bin a) (V2 a)
bin_ = iso getBin Bin

l_ :: Lens' (Bin a) a
l_ = bin_._x

r_ :: Lens' (Bin a) a
r_ = bin_._y


type Quadtree = B Quad

-- | Quaternary nodes.
--
-- Mnemonic for fields: bottom/top, left/right.
newtype Quad a = Quad { getQuad :: V2 (V2 a) }
  deriving (Foldable, Functor, Generic, Generic1, Monoid, Semigroup, Traversable)
  deriving (Applicative) via (V2 :.: V2)

instance FoldableWithIndex (V2 Bit) Quad
instance FunctorWithIndex (V2 Bit) Quad
instance TraversableWithIndex (V2 Bit) Quad where
  itraverse f (Quad q) = Quad <$> itraverse (\ ix -> itraverse (\ iy -> f (indices^.el ix.el iy))) q
    where
    indices = head (deinterleaveWith V2 (deinterleaveWith V2 (V2 <$> [I0, I1] <*> [I0, I1])))

instance UnfoldableWithIndex (V2 Bit) Quad where
  iunfoldA f = quad
    <$> f (V2 I0 I0)
    <*> f (V2 I1 I0)
    <*> f (V2 I0 I1)
    <*> f (V2 I1 I1)

instance Indexed (V2 Bit) Quad where
  q ! i = case i of
    V2 I0 I0 -> q^.bl_
    V2 I1 I0 -> q^.br_
    V2 I0 I1 -> q^.tl_
    V2 I1 I1 -> q^.tr_

instance MutableIndexed (V2 Bit) Quad where
  insert (V2 I0 I0) = set bl_
  insert (V2 I1 I0) = set br_
  insert (V2 I0 I1) = set tl_
  insert (V2 I1 I1) = set tr_

instance Linear.Finite Quad where
  type Size Quad = 4

  fromV (Linear.V v) = Quad (head (deinterleaveWith V2 (deinterleaveWith V2 (map (v V.!) [0..3]))))

quad :: a -> a -> a -> a -> Quad a
quad bl br tl tr = Quad $ V2 (V2 bl br) (V2 tl tr)

quad_ :: Iso' (Quad a) (V2 (V2 a))
quad_ = iso getQuad Quad

bl_ :: Lens' (Quad a) a
bl_ = quad_._x._x

br_ :: Lens' (Quad a) a
br_ = quad_._x._y

tl_ :: Lens' (Quad a) a
tl_ = quad_._y._x

tr_ :: Lens' (Quad a) a
tr_ = quad_._y._y


type Octree = B Oct

-- | Octonary nodes.
--
-- Mnemonic for fields: bottom/top, left/right, near/far.
newtype Oct a = Oct { getOct :: V2 (V2 (V2 a)) }
  deriving (Foldable, Functor, Generic, Generic1, Monoid, Semigroup, Traversable)
  deriving (Applicative) via (V2 :.: V2 :.: V2)

instance FoldableWithIndex (V3 Bit) Oct
instance FunctorWithIndex (V3 Bit) Oct
instance TraversableWithIndex (V3 Bit) Oct where
  itraverse f (Oct o) = Oct <$> itraverse (\ ix -> itraverse (\ iy -> let ixy = el ix.el iy in itraverse (\ iz -> f (indices^.ixy.el iz)))) o
    where
    indices = head (deinterleaveWith V2 (deinterleaveWith V2 (deinterleaveWith V2 (V3 <$> [I0, I1] <*> [I0, I1] <*> [I0, I1]))))

instance UnfoldableWithIndex (V3 Bit) Oct where
  iunfoldA f = oct
    <$> f (V3 I0 I0 I0)
    <*> f (V3 I1 I0 I0)
    <*> f (V3 I0 I1 I0)
    <*> f (V3 I1 I1 I0)
    <*> f (V3 I0 I0 I1)
    <*> f (V3 I1 I0 I1)
    <*> f (V3 I0 I1 I1)
    <*> f (V3 I1 I1 I1)

instance Indexed (V3 Bit) Oct where
  o ! i = case i of
    V3 I0 I0 I0 -> o^.bln_
    V3 I1 I0 I0 -> o^.brn_
    V3 I0 I1 I0 -> o^.tln_
    V3 I1 I1 I0 -> o^.trn_
    V3 I0 I0 I1 -> o^.blf_
    V3 I1 I0 I1 -> o^.brf_
    V3 I0 I1 I1 -> o^.tlf_
    V3 I1 I1 I1 -> o^.trf_

instance MutableIndexed (V3 Bit) Oct where
  insert (V3 I0 I0 I0) = set bln_
  insert (V3 I1 I0 I0) = set brn_
  insert (V3 I0 I1 I0) = set tln_
  insert (V3 I1 I1 I0) = set trn_
  insert (V3 I0 I0 I1) = set blf_
  insert (V3 I1 I0 I1) = set brf_
  insert (V3 I0 I1 I1) = set tlf_
  insert (V3 I1 I1 I1) = set trf_

instance Linear.Finite Oct where
  type Size Oct = 8

  fromV (Linear.V v) = Oct (head (deinterleaveWith V2 (deinterleaveWith V2 (deinterleaveWith V2 (map (v V.!) [0..7])))))

oct :: a -> a -> a -> a -> a -> a -> a -> a -> Oct a
oct bln brn tln trn blf brf tlf trf = Oct $ V2 (V2 (V2 bln brn) (V2 tln trn)) (V2 (V2 blf brf) (V2 tlf trf))

oct_ :: Iso' (Oct a) (V2 (V2 (V2 a)))
oct_ = iso getOct Oct

bln_ :: Lens' (Oct a) a
bln_ = oct_._x._x._x

brn_ :: Lens' (Oct a) a
brn_ = oct_._x._x._y

tln_ :: Lens' (Oct a) a
tln_ = oct_._x._y._x

trn_ :: Lens' (Oct a) a
trn_ = oct_._x._y._y

blf_ :: Lens' (Oct a) a
blf_ = oct_._x._x._y

brf_ :: Lens' (Oct a) a
brf_ = oct_._y._x._y

tlf_ :: Lens' (Oct a) a
tlf_ = oct_._x._y._y

trf_ :: Lens' (Oct a) a
trf_ = oct_._y._y._y


tetra :: SparseUnfoldableWithIndex V3 (Index s) (B Oct s) => (V3 (Index s) -> a) -> B Oct s a
tetra = run . iunfoldSparseM (pure . fromBit . foldl' xor I0) . (pure .)


-- | Unfolding of finite dense structures with an index.
class UnfoldableWithIndex i f | f -> i where
  iunfoldA :: Applicative m => (i -> m b) -> m (f b)

instance UnfoldableWithIndex (E V1) V1 where
  iunfoldA coalg = V1 <$> coalg ex

instance UnfoldableWithIndex (E V2) V2 where
  iunfoldA coalg = V2 <$> coalg ex <*> coalg ey

instance UnfoldableWithIndex (E V3) V3 where
  iunfoldA coalg = V3 <$> coalg ex <*> coalg ey <*> coalg ez


iunfoldr :: UnfoldableWithIndex i f => (i -> s -> (s, b)) -> s -> f b
iunfoldr f a = run . evalState a . iunfoldA $ state . f

-- | Unfolding of finite sparse structures with an index.
class SparseUnfoldableWithIndex v i t | t -> v i where
  iunfoldSparseM :: Monad m => (v Bit -> m Bool) -> (v i -> m a) -> m (t a)


class BinaryIndexed f t | t -> f where
  indices :: t (f Bit)

instance BinaryIndexed V1 Bin where
  indices = Bin (head (deinterleaveWith V2 (map V1 [I0, I1])))

instance BinaryIndexed V2 Quad where
  indices = Quad (head (deinterleaveWith V2 (deinterleaveWith V2 (liftA2 V2 [I0, I1] [I0, I1]))))

instance BinaryIndexed V3 Oct where
  indices = Oct (head (deinterleaveWith V2 (deinterleaveWith V2 (deinterleaveWith V2 (liftA3 V3 [I0, I1] [I0, I1] [I0, I1])))))

instance BinaryIndexed V1 V2 where
  indices = head (deinterleaveWith V2 (map V1 [I0, I1]))

instance BinaryIndexed V2 (V2 :.: V2) where
  indices = C (head (deinterleaveWith V2 (deinterleaveWith V2 (liftA2 V2 [I0, I1] [I0, I1]))))

instance BinaryIndexed V3 (V2 :.: V2 :.: V2) where
  indices = C (head (deinterleaveWith V2 (deinterleaveWith (fmap C . V2) (deinterleaveWith V2 (liftA3 V3 [I0, I1] [I0, I1] [I0, I1])))))


class Indexed i f | f -> i where
  (!) :: f a -> i -> a
  infixl 9 !

class SparseIndexed i f | f -> i where
  (!?) :: f a -> i -> Maybe a
  infixl 9 !?

class MutableIndexed i f | f -> i where
  insert :: i -> a -> f a -> f a


deinterleaveWith :: (a -> a -> b) -> [a] -> [b]
deinterleaveWith f (x:y:t) = f x y : deinterleaveWith f t
deinterleaveWith _ _       = []
