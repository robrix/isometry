{-# LANGUAGE DataKinds #-}
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
import           Control.Lens.Indexed hiding (Indexed(..), indices)
import           Data.Bin.Bit
import           Data.Bin.Index
import           Data.Bin.Shape
import           Data.Bits
import           Data.Coerce
import           Data.Foldable (foldl')
import           Data.Functor.C
import           Data.Functor.Identity
import           Data.Monoid (Sum(..))
import           Data.Proxy
import           GHC.TypeLits
import qualified Linear.V as Linear
import           Linear.V1
import           Linear.V2
import           Linear.V3
import           Linear.Vector (E)

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
  ifoldMap f (L   a) = f (pure il) a
  ifoldMap f (B _ b) = ifoldMap (\ i -> ifoldMap (\ j -> f (ib <$> i <*> j))) b

instance (FunctorWithIndex (v Bit) f, Applicative v) => FunctorWithIndex (v (Index s)) (B f s) where
  imap _ E       = E
  imap f (L   a) = L (f (pure il) a)
  imap f (B l b) = B l (imap (\ i -> imap (\ j -> f (ib <$> i <*> j))) b)

instance (FoldableWithIndex (v Bit) f, FunctorWithIndex (v Bit) f, TraversableWithIndex (v Bit) f, Applicative v) => TraversableWithIndex (v (Index s)) (B f s) where
  itraverse _ E       = pure E
  itraverse f (L   a) = L <$> f (pure il) a
  itraverse f (B l b) = B l <$> itraverse (\ i -> itraverse (\ j -> f (ib <$> i <*> j))) b

-- | Note that this instance can only express dense unfoldings.
instance (UnfoldableWithIndex (v Bit) f, Applicative v) => UnfoldableWithIndex (v (Index 'S1)) (B f 'S1) where
  iunfoldA f = L <$> f (pure il)

-- | Note that this instance can only express dense unfoldings.
instance (UnfoldableWithIndex (v Bit) f, Applicative v, UnfoldableWithIndex (v (Index s)) (B f s), Foldable f) => UnfoldableWithIndex (v (Index ('S2x s))) (B f ('S2x s)) where
  iunfoldA f = makeB <$> iunfoldA (\ i -> iunfoldA (\ j -> f (ib <$> i <*> j)))

instance (Applicative v, UnfoldableWithIndex (v Bit) f) => SparseUnfoldableWithIndex v (Index 'S1) (B f 'S1) where
  iunfoldSparseM _ leaf = L <$> leaf (pure il)

instance (Applicative v, UnfoldableWithIndex (v Bit) f, SparseUnfoldableWithIndex v (Index s) (B f s), Foldable f) => SparseUnfoldableWithIndex v (Index ('S2x s)) (B f ('S2x s)) where
  iunfoldSparseM branch leaf = b <$> iunfoldA (\ i -> branch i >>= \ b -> if b then iunfoldSparseM branch (leaf . (ib <$> i <*>)) else pure E)

instance (Indexed (v Bit) f, Functor v) => SparseIndexed (v (Index s)) (B f s) where
  E     !? _ = Nothing
  L   a !? _ = Just a
  B _ b !? v = b ! fmap (fst . decompose) v !? fmap (snd . decompose) v

instance MutableIndexed (v Bit) f => MutableIndexed (v (Index 'S1)) (B f 'S1) where
  insert _ a _ = L a

instance (MutableIndexed (v Bit) f, Applicative f, Foldable f, Functor v, MutableIndexed (v (Index s)) (B f s), Indexed (v Bit) f) => MutableIndexed (v (Index ('S2x s))) (B f ('S2x s)) where
  insert i a = makeB . uncurry (insert ihead . insert itail a) . \case
    E     -> (E, pure E)
    B _ f -> (f ! ihead, f)
    where
    ihead = fst . decompose <$> i
    itail = snd . decompose <$> i

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

capacity :: forall s f a . (KnownNat (Linear.Size f), KnownNat (Size s)) => B f s a -> Int
capacity b = size b ^ (round (logBase @Float 2 (fromIntegral (natVal (Proxy @(Linear.Size f))))) :: Int)


tetra :: (Foldable v, SparseUnfoldableWithIndex v i t) => (v i -> a) -> t a
tetra = iunfoldSparse (fromBit . foldl' xor B0)
{-# INLINABLE tetra #-}


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
  iunfoldSparse :: (v Bit -> Bool) -> (v i -> a) -> t a
  iunfoldSparse branch (leaf :: v i -> a) = coerce (iunfoldSparseM :: (v Bit -> Identity Bool) -> (v i -> Identity a) -> Identity (t a)) branch leaf


class BinaryIndexed f t | t -> f where
  indices :: t (f Bit)

instance BinaryIndexed V1 V2 where
  indices = head (deinterleaveWith V2 (map V1 [B0, B1]))

instance BinaryIndexed V2 (V2 :.: V2) where
  indices = C (head (deinterleaveWith V2 (deinterleaveWith V2 (liftA2 V2 [B0, B1] [B0, B1]))))

instance BinaryIndexed V3 (V2 :.: V2 :.: V2) where
  indices = C (head (deinterleaveWith V2 (deinterleaveWith (fmap C . V2) (deinterleaveWith V2 (liftA3 V3 [B0, B1] [B0, B1] [B0, B1])))))


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
