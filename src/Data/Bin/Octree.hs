{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
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
import Control.Monad (foldM_)
import Data.Bin.Bit
import Data.Bin.Index
import Data.Bin.Shape
import Data.Unfoldable (SparseUnfoldableWithIndex(..))
import Foreign.Marshal.Array.Lift
import Foreign.Ptr
import Foreign.Storable
import GHC.Exts
import Linear.V3

data Octree s a where
  E :: Octree s a
  L :: !a -> Octree 'Z a
  B :: {-# UNPACK #-} !Int
    -> !(Octree s a) -> !(Octree s a)
    -> !(Octree s a) -> !(Octree s a)
    -> !(Octree s a) -> !(Octree s a)
    -> !(Octree s a) -> !(Octree s a)
    -> Octree ('S s) a

deriving instance Show a => Show (Octree s a)

instance Foldable (Octree s) where
  foldMap (f :: a -> m) = go
    where
    go :: Octree s' a -> m
    go = \case
      E   -> mempty
      L a -> f a
      B _ lbf rbf ltf rtf lbn rbn ltn rtn -> go lbf <> go rbf <> go ltf <> go rtf <> go lbn <> go rbn <> go ltn <> go rtn
  {-# INLINE foldMap #-}

  foldr (f :: a -> b -> b) = go
    where
    go :: b -> Octree s' a -> b
    go z = \case
      E   -> z
      L a -> f a z
      B _ lbf rbf ltf rtf lbn rbn ltn rtn -> go (go (go (go (go (go (go (go z rtn) ltn) rbn) lbn) rtf) ltf) rbf) lbf
  {-# INLINE foldr #-}

  length = \case
    E                   -> 0
    L _                 -> 1
    B l _ _ _ _ _ _ _ _ -> l
  {-# INLINE length #-}

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
  {-# INLINE ifoldMap #-}

instance SparseUnfoldableWithIndex (V3 Bit) (V3 (Index 'Z)) (Octree 'Z) where
  iunfoldSparseM _ leaf = L <$> leaf (pure il)
  {-# INLINE iunfoldSparseM #-}

  iunfoldSparse _ leaf = L (leaf (pure il))
  {-# INLINE iunfoldSparse #-}

instance SparseUnfoldableWithIndex (V3 Bit) (V3 (Index s)) (Octree s) => SparseUnfoldableWithIndex (V3 Bit) (V3 (Index ('S s))) (Octree ('S s)) where
  iunfoldSparseM branch leaf = b
    <$> go (V3 B0 B0 B0) <*> go (V3 B1 B0 B0)
    <*> go (V3 B0 B1 B0) <*> go (V3 B1 B1 B0)
    <*> go (V3 B0 B0 B1) <*> go (V3 B1 B0 B1)
    <*> go (V3 B0 B1 B1) <*> go (V3 B1 B1 B1)
    where
    go i = branch i >>= \ b -> if b then iunfoldSparseM branch (leaf . (ib <$> i <*>)) else pure E
  {-# INLINE iunfoldSparseM #-}

  iunfoldSparse branch leaf = b
    (go (V3 B0 B0 B0)) (go (V3 B1 B0 B0))
    (go (V3 B0 B1 B0)) (go (V3 B1 B1 B0))
    (go (V3 B0 B0 B1)) (go (V3 B1 B0 B1))
    (go (V3 B0 B1 B1)) (go (V3 B1 B1 B1))
    where
    go i = if branch i then iunfoldSparse branch (leaf . (ib <$> i <*>)) else E
  {-# INLINE iunfoldSparse #-}

instance Applicative (Octree 'Z) where
  pure = L

  E     <*> _ = E
  L   f <*> a = fmap f a

instance Applicative (Octree s) => Applicative (Octree ('S s)) where
  pure a = b (pure a) (pure a) (pure a) (pure a) (pure a) (pure a) (pure a) (pure a)

  E     <*> _     = E
  _     <*> E     = E
  B _ f1 f2 f3 f4 f5 f6 f7 f8 <*> B _ a1 a2 a3 a4 a5 a6 a7 a8 = b (f1 <*> a1) (f2 <*> a2) (f3 <*> a3) (f4 <*> a4) (f5 <*> a5) (f6 <*> a6) (f7 <*> a7) (f8 <*> a8)

b :: Octree s a -> Octree s a -> Octree s a -> Octree s a -> Octree s a -> Octree s a -> Octree s a -> Octree s a -> Octree ('S s) a
b lbf rbf ltf rtf lbn rbn ltn rtn
  | len > 0   = B len lbf rbf ltf rtf lbn rbn ltn rtn
  | otherwise = E
  where
  !len = length lbf + length rbf + length ltf + length rtf + length lbn + length rbn + length ltn + length rtn
{-# INLINE b #-}

withOctreeLen :: (Has (Lift IO) sig m, Storable a) => Octree s a -> (Int -> Ptr a -> m b) -> m b
withOctreeLen o with = allocaArray len $ \ p -> do
  sendIO $ foldM_ (\ !off !a -> do
    pokeElemOff p off a
    pure $! off + 1) 0 o
  with len p
  where
  len = length o

withOctreeLen2 :: (Has (Lift IO) sig m, Storable b, Storable c) => Octree s a -> (a -> (b, c)) -> (Int -> Ptr b -> Ptr c -> m r) -> m r
withOctreeLen2 o (prj :: a -> (b, c)) with = allocaArray len $ \ !pb -> allocaArray len $ \ !pc -> do
  let go :: Octree s' a -> (Int# -> IO ()) -> Int# -> IO ()
      go E     k = k
      go (L a) k = \ n# -> do
        let (!b, !c) = prj a
        pokeElemOff pb (I# n#) b
        pokeElemOff pc (I# n#) c
        k (n# +# 1#)
      go (B _ lbf rbf ltf rtf lbn rbn ltn rtn) k = go lbf (go rbf (go ltf (go rtf (go lbn (go rbn (go ltn (go rtn k)))))))
  sendIO $ go o (\ _ -> pure ()) 0#
  with len pb pc
  where
  !len = length o
