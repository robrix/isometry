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
( Shape(..)
, S2
, S4
, S8
, S16
, S32
, S64
, S128
, S256
, S512
, S1024
, S2048
, S4096
, S8192
, Size
, Index(..)
, fromIndex
, toFraction
, B(..)
, isE
, b
, size
, capacity
, Bin(..)
, l_
, r_
, Quad(..)
, quad
, bl_
, br_
, tl_
, tr_
, Oct(..)
, oct
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
, Indexed(..)
, SparseIndexed(..)
, MutableIndexed(..)
) where

import           Control.Carrier.State.Church
import           Control.Lens (Lens', coerced, iso, set, (^.))
import           Control.Lens.Indexed hiding (Indexed(..))
import           Data.Bin.Bit
import           Data.Bits
import           Data.Functor.C
import           Data.Proxy
import qualified Data.Vector as V
import           GHC.Generics (Generic, Generic1)
import           GHC.TypeLits
import qualified Linear.V as Linear
import           Linear.V1
import           Linear.V2
import           Linear.V3
import           Linear.Vector (E)

-- | The shape of (non-empty) perfectly balanced binary trees.
--
-- This represents shapes of size 2ⁿ.
data Shape
  = S1         -- 1
  | S2x !Shape -- 2 * n

type S2 = 'S2x 'S1
type S4 = 'S2x S2
type S8 = 'S2x S4
type S16 = 'S2x S8
type S32 = 'S2x S16
type S64 = 'S2x S32
type S128 = 'S2x S64
type S256 = 'S2x S128
type S512 = 'S2x S256
type S1024 = 'S2x S512
type S2048 = 'S2x S1024
type S4096 = 'S2x S2048
type S8192 = 'S2x S4096


type family Size (b :: Shape) :: Nat where
  Size 'S1      = 1
  Size ('S2x l) = 2 * Size l


data Index i where
  IL :: Index 'S1
  IB :: Bit -> Index i -> Index ('S2x i)

deriving instance Eq   (Index i)
deriving instance Ord  (Index i)
deriving instance Show (Index i)

fromIndex :: Index ('S2x i) -> (Bit, Index i)
fromIndex (IB b i) = (b, i)

toFraction :: Index i -> (Integer, Integer)
toFraction = go
  where
  go :: Index i -> (Integer, Integer)
  go IL       = (0, 1)
  go (IB b i) = let (n, d) = go i in case b of
    I0 -> (n, d * 2)
    I1 -> (n + d, d * 2)


data B s f a where
  E :: B s f a
  L :: !a -> B 'S1 f a
  B :: !(f (B s f a)) -> B ('S2x s) f a

deriving instance Foldable f => Foldable (B s f)
deriving instance Functor f => Functor (B s f)
deriving instance Traversable f => Traversable (B s f)

instance (FoldableWithIndex (v Bit) f, Applicative v) => FoldableWithIndex (v (Index s)) (B s f) where
  ifoldMap _ E     = mempty
  ifoldMap f (L a) = f (pure IL) a
  ifoldMap f (B b) = ifoldMap (\ i -> ifoldMap (\ j -> f (IB <$> i <*> j))) b

instance (FunctorWithIndex (v Bit) f, Applicative v) => FunctorWithIndex (v (Index s)) (B s f) where
  imap _ E     = E
  imap f (L a) = L (f (pure IL) a)
  imap f (B b) = B (imap (\ i -> imap (\ j -> f (IB <$> i <*> j))) b)

instance (FoldableWithIndex (v Bit) f, FunctorWithIndex (v Bit) f, TraversableWithIndex (v Bit) f, Applicative v) => TraversableWithIndex (v (Index s)) (B s f) where
  itraverse _ E     = pure E
  itraverse f (L a) = L <$> f (pure IL) a
  itraverse f (B b) = B <$> itraverse (\ i -> itraverse (\ j -> f (IB <$> i <*> j))) b

-- | Note that this instance can only express dense unfoldings.
instance (UnfoldableWithIndex (v Bit) f, Applicative v) => UnfoldableWithIndex (v (Index 'S1)) (B 'S1 f) where
  iunfoldA f = L <$> f (pure IL)

-- | Note that this instance can only express dense unfoldings.
instance (UnfoldableWithIndex (v Bit) f, Applicative v, UnfoldableWithIndex (v (Index s)) (B s f)) => UnfoldableWithIndex (v (Index ('S2x s))) (B ('S2x s) f) where
  iunfoldA f = B <$> iunfoldA (\ i -> iunfoldA (\ j -> f (IB <$> i <*> j)))

instance (UnfoldableWithIndex (v Bit) f, Applicative v) => SparseUnfoldableWithIndex (v (Index 'S1)) (B 'S1 f) where
  iunfoldSparseA f = maybe E L <$> f (pure IL)

instance (Foldable f, UnfoldableWithIndex (v Bit) f, Applicative v, SparseUnfoldableWithIndex (v (Index s)) (B s f)) => SparseUnfoldableWithIndex (v (Index ('S2x s))) (B ('S2x s) f) where
  iunfoldSparseA f = b <$> iunfoldA (\ i -> iunfoldSparseA (\ j -> f (IB <$> i <*> j)))

instance (Indexed (v Bit) f, Functor v) => SparseIndexed (v (Index s)) (B s f) where
  E   !? _ = Nothing
  L a !? _ = Just a
  B b !? v = b ! fmap (fst . fromIndex) v !? fmap (snd . fromIndex) v

instance MutableIndexed (v Bit) f => MutableIndexed (v (Index 'S1)) (B 'S1 f) where
  insert _ a _ = L a

instance (MutableIndexed (v Bit) f, Applicative f, Functor v, MutableIndexed (v (Index s)) (B s f), Indexed (v Bit) f) => MutableIndexed (v (Index ('S2x s))) (B ('S2x s) f) where
  insert i a = B . uncurry (insert ihead . insert itail a) . \case
    E   -> (E, pure E)
    B f -> (f ! ihead, f)
    where
    ihead = fst . fromIndex <$> i
    itail = snd . fromIndex <$> i

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

isE :: B s f a -> Bool
isE E = True
isE _ = False

b :: Foldable f => f (B s f a) -> B ('S2x s) f a
b f | all isE f = E
    | otherwise = B f

size :: forall s f a . KnownNat (Size s) => B s f a -> Integer
size _ = natVal (Proxy @(Size s))

capacity :: forall s f a . (KnownNat (Linear.Size f), KnownNat (Size s)) => B s f a -> Integer
capacity b = size b ^ (round (logBase @Float 2 (fromIntegral (natVal (Proxy @(Linear.Size f))))) :: Int)


-- | Binary nodes.
--
-- Mnemonic for fields: left/right.
newtype Bin a = Bin { getBin :: V2 a }
  deriving (Applicative, Foldable, Functor, Generic, Generic1, Monoid, Semigroup, Traversable)

instance FoldableWithIndex (V1 Bit) Bin
instance FunctorWithIndex (V1 Bit) Bin
instance TraversableWithIndex (V1 Bit) Bin where
  itraverse f (Bin (V2 l r)) = bin <$> f (V1 I0) l <*> f (V1 I1) r

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

  fromV (Linear.V v) = bin (v V.! 0) (v V.! 1)

instance R1 Bin where
  _x = iso getBin Bin ._x

bin :: a -> a -> Bin a
bin l r = Bin $ V2 l r

l_ :: Lens' (Bin a) a
l_ = iso getBin Bin ._x

r_ :: Lens' (Bin a) a
r_ = iso getBin Bin ._y


-- | Quaternary nodes.
--
-- Mnemonic for fields: bottom/top, left/right.
newtype Quad a = Quad { getQuad :: Bin (Bin a) }
  deriving (Foldable, Functor, Generic, Generic1, Monoid, Semigroup, Traversable)
  deriving (Applicative) via (Bin :.: Bin)

instance FoldableWithIndex (V2 Bit) Quad
instance FunctorWithIndex (V2 Bit) Quad
instance TraversableWithIndex (V2 Bit) Quad where
  itraverse f (Quad (Bin (V2 (Bin (V2 bl br)) (Bin (V2 tl tr))))) = quad <$> f (V2 I0 I0) bl <*> f (V2 I1 I0) br <*> f (V2 I0 I1) tl <*> f (V2 I1 I1) tr

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

  fromV (Linear.V v) = quad (v V.! 0) (v V.! 1) (v V.! 2) (v V.! 3)

quad :: a -> a -> a -> a -> Quad a
quad bl br tl tr = Quad $ bin (bin bl br) (bin tl tr)

bl_ :: Lens' (Quad a) a
bl_ = coerced.l_.l_

br_ :: Lens' (Quad a) a
br_ = coerced.l_.r_

tl_ :: Lens' (Quad a) a
tl_ = coerced.r_.l_

tr_ :: Lens' (Quad a) a
tr_ = coerced.r_.r_


-- | Octonary nodes.
--
-- Mnemonic for fields: bottom/top, left/right, near/far.
newtype Oct a = Oct { getOct :: Bin (Bin (Bin a)) }
  deriving (Foldable, Functor, Generic, Generic1, Monoid, Semigroup, Traversable)
  deriving (Applicative) via (Bin :.: Bin :.: Bin)

instance FoldableWithIndex (V3 Bit) Oct
instance FunctorWithIndex (V3 Bit) Oct
instance TraversableWithIndex (V3 Bit) Oct where
  itraverse f (Oct (Bin (V2 (Bin (V2 (Bin (V2 bln brn)) (Bin (V2 tln trn)))) (Bin (V2 (Bin (V2 blf brf)) (Bin (V2 tlf trf))))))) = oct
    <$> f (V3 I0 I0 I0) bln <*> f (V3 I1 I0 I0) brn
    <*> f (V3 I0 I1 I0) tln <*> f (V3 I1 I1 I0) trn
    <*> f (V3 I0 I0 I1) blf <*> f (V3 I1 I0 I1) brf
    <*> f (V3 I0 I1 I1) tlf <*> f (V3 I1 I1 I1) trf

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

  fromV (Linear.V v) = oct (v V.! 0) (v V.! 1) (v V.! 2) (v V.! 3) (v V.! 4) (v V.! 5) (v V.! 6) (v V.! 7)

oct :: a -> a -> a -> a -> a -> a -> a -> a -> Oct a
oct bln brn tln trn blf brf tlf trf = Oct $ bin (bin (bin bln brn) (bin tln trn)) (bin (bin blf brf) (bin tlf trf))

bln_ :: Lens' (Oct a) a
bln_ = coerced.l_.l_.l_

brn_ :: Lens' (Oct a) a
brn_ = coerced.l_.l_.r_

tln_ :: Lens' (Oct a) a
tln_ = coerced.l_.r_.l_

trn_ :: Lens' (Oct a) a
trn_ = coerced.l_.r_.r_

blf_ :: Lens' (Oct a) a
blf_ = coerced.l_.l_.r_

brf_ :: Lens' (Oct a) a
brf_ = coerced.r_.l_.r_

tlf_ :: Lens' (Oct a) a
tlf_ = coerced.l_.r_.r_

trf_ :: Lens' (Oct a) a
trf_ = coerced.r_.r_.r_


class UnfoldB s where
  unfoldB :: (V3 Bit -> Bool) -> (V3 (Index s) -> a) -> B s Oct a

instance UnfoldB 'S1 where
  unfoldB _ leaf = L (leaf (pure IL))

instance UnfoldB s => UnfoldB ('S2x s) where
  unfoldB branch leaf = b (run (iunfoldA (\ i -> pure $ if branch i then unfoldB branch (leaf . (IB <$> i <*>)) else E)))


tetra :: UnfoldB s => (V3 (Index s) -> a) -> B s Oct a
tetra = unfoldB (fromBit . foldl1 xor)


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
class UnfoldableWithIndex i f => SparseUnfoldableWithIndex i f where
  iunfoldSparseA :: Applicative m => (i -> m (Maybe b)) -> m (f b)


class Indexed i f | f -> i where
  (!) :: f a -> i -> a
  infixl 9 !

class SparseIndexed i f | f -> i where
  (!?) :: f a -> i -> Maybe a
  infixl 9 !?

class MutableIndexed i f | f -> i where
  insert :: i -> a -> f a -> f a
