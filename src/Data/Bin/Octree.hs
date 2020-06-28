{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Bin.Octree
( Octree(..)
, withOctreeLen
, withOctreeLen2
) where

import Control.Carrier.Lift
import Control.Lens.Indexed
import Data.Bin.Bit
import Data.Foldable (for_)
import Data.IORef
import Data.Bin.Index
import Data.Bin.Shape
import Data.Bin.Tree (SparseUnfoldableWithIndex(..))
import Foreign.Marshal.Array.Lift
import Foreign.Ptr
import Foreign.Storable
import Linear.V3

data Octree s a where
  E :: Octree s a
  L :: !a -> Octree 'S1 a
  B :: {-# UNPACK #-} !Int
    -> !(Octree s a) -> !(Octree s a)
    -> !(Octree s a) -> !(Octree s a)
    -> !(Octree s a) -> !(Octree s a)
    -> !(Octree s a) -> !(Octree s a)
    -> Octree ('S2x s) a

instance Foldable (Octree s) where
  foldMap (f :: a -> m) = go
    where
    go :: Octree s' a -> m
    go = \case
      E   -> mempty
      L a -> f a
      B _ lbf rbf ltf rtf lbn rbn ltn rtn -> foldMap f lbf <> foldMap f rbf <> foldMap f ltf <> foldMap f rtf <> foldMap f lbn <> foldMap f rbn <> foldMap f ltn <> foldMap f rtn
  {-# INLINABLE foldMap #-}

  length = \case
    E                   -> 0
    L _                 -> 1
    B l _ _ _ _ _ _ _ _ -> l
  {-# INLINABLE length #-}

deriving instance Functor     (Octree s)
deriving instance Traversable (Octree s)

instance FoldableWithIndex (V3 (Index s)) (Octree s) where
  ifoldMap f = \case
    E     -> mempty
    L   a -> f (pure il) a
    B _ lbf rbf ltf rtf lbn rbn ltn rtn
      -> go B0 B0 B0 lbf <> go B1 B0 B0 rbf
      <> go B0 B1 B0 ltf <> go B1 B1 B0 rtf
      <> go B0 B0 B1 lbn <> go B1 B0 B1 rbn
      <> go B0 B1 B1 ltn <> go B1 B1 B1 rtn
      where
      go x y z = ifoldMap (f . (ib <$> V3 x y z <*>))
  {-# INLINABLE ifoldMap #-}

instance SparseUnfoldableWithIndex V3 (Index 'S1) (Octree 'S1) where
  iunfoldSparseM _ leaf = L <$> leaf (pure il)
  {-# INLINABLE iunfoldSparseM #-}

instance SparseUnfoldableWithIndex V3 (Index s) (Octree s) => SparseUnfoldableWithIndex V3 (Index ('S2x s)) (Octree ('S2x s)) where
  iunfoldSparseM branch leaf = makeB
    <$> go (V3 B0 B0 B0) <*> go (V3 B1 B0 B0)
    <*> go (V3 B0 B1 B0) <*> go (V3 B1 B1 B0)
    <*> go (V3 B0 B0 B1) <*> go (V3 B1 B0 B1)
    <*> go (V3 B0 B1 B1) <*> go (V3 B1 B1 B1)
    where
    go i = branch i >>= \ b -> if b then iunfoldSparseM branch (leaf . (ib <$> i <*>)) else pure E
  {-# INLINABLE iunfoldSparseM #-}

instance Applicative (Octree 'S1) where
  pure = L

  E     <*> _ = E
  L   f <*> a = fmap f a

instance Applicative (Octree s) => Applicative (Octree ('S2x s)) where
  pure a = makeB (pure a) (pure a) (pure a) (pure a) (pure a) (pure a) (pure a) (pure a)

  E     <*> _     = E
  _     <*> E     = E
  B _ f1 f2 f3 f4 f5 f6 f7 f8 <*> B _ a1 a2 a3 a4 a5 a6 a7 a8 = makeB (f1 <*> a1) (f2 <*> a2) (f3 <*> a3) (f4 <*> a4) (f5 <*> a5) (f6 <*> a6) (f7 <*> a7) (f8 <*> a8)

makeB :: Octree s a -> Octree s a -> Octree s a -> Octree s a -> Octree s a -> Octree s a -> Octree s a -> Octree s a -> Octree ('S2x s) a
makeB o1 o2 o3 o4 o5 o6 o7 o8 = B (length o1 + length o2 + length o3 + length o4 + length o5 + length o6 + length o7 + length o8) o1 o2 o3 o4 o5 o6 o7 o8
{-# INLINABLE makeB #-}

withOctreeLen :: (Has (Lift IO) sig m, Storable a) => Octree s a -> (Int -> Ptr a -> m b) -> m b
withOctreeLen o with = allocaArray len $ \ p -> do
  ref <- sendIO $ newIORef 0
  _ <- sendIO . for_ o $ \ a -> do
    !off <- readIORef ref
    pokeElemOff p off a
    writeIORef ref $ off + 1
  with len p
  where
  len = length o

withOctreeLen2 :: forall a b c r s m sig . (Has (Lift IO) sig m, Storable b, Storable c) => Octree s a -> (a -> (b, c)) -> (Int -> Ptr b -> Ptr c -> m r) -> m r
withOctreeLen2 o prj with = allocaArray len $ \ pb -> allocaArray len $ \ pc -> do
  ref <- sendIO $ newIORef 0
  sendIO . for_ o $ \ a -> do
    !off <- readIORef ref
    let (!b, !c) = prj a
    pokeElemOff pb off b
    pokeElemOff pc off c
    writeIORef ref $ off + 1
  with len pb pc
  where
  !len = length o
