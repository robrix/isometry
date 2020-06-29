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
module Data.Bin.Quadtree
( Quadtree(..)
) where

import Control.Lens.Indexed
import Data.Bin.Bit
import Data.Bin.Index
import Data.Bin.Shape
import Data.Bin.Tree (SparseUnfoldableWithIndex(..))
import Linear.V2

data Quadtree s a where
  E :: Quadtree s a
  L :: !a -> Quadtree 'S1 a
  B :: {-# UNPACK #-} !Int
    -> !(Quadtree s a) -> !(Quadtree s a)
    -> !(Quadtree s a) -> !(Quadtree s a)
    -> Quadtree ('S2x s) a

instance Foldable (Quadtree s) where
  foldMap (f :: a -> m) = go
    where
    go :: Quadtree s' a -> m
    go = \case
      E       -> mempty
      L a     -> f a
      B _ lb rb lt rt -> go lb <> go rb <> go lt <> go rt
  {-# INLINABLE foldMap #-}

  length = \case
    E           -> 0
    L _         -> 1
    B l _ _ _ _ -> l
  {-# INLINABLE length #-}

deriving instance Functor     (Quadtree s)
deriving instance Traversable (Quadtree s)

instance FoldableWithIndex (V2 (Index s)) (Quadtree s) where
  ifoldMap f = \case
    E     -> mempty
    L   a -> f (pure il) a
    B _ lb rb lt rt
      -> go (V2 B0 B0) lb <> go (V2 B1 B0) rb
      <> go (V2 B0 B1) lt <> go (V2 B1 B1) rt
      where
      go b = ifoldMap (f . (ib <$> b <*>))
  {-# INLINABLE ifoldMap #-}

instance SparseUnfoldableWithIndex (V2 Bit) (V2 (Index 'S1)) (Quadtree 'S1) where
  iunfoldSparseM _ leaf = L <$> leaf (pure il)
  {-# INLINABLE iunfoldSparseM #-}

  iunfoldSparse _ leaf = L (leaf (pure il))
  {-# INLINABLE iunfoldSparse #-}

instance SparseUnfoldableWithIndex (V2 Bit) (V2 (Index s)) (Quadtree s) => SparseUnfoldableWithIndex (V2 Bit) (V2 (Index ('S2x s))) (Quadtree ('S2x s)) where
  iunfoldSparseM branch leaf = b <$> go (V2 B0 B0) <*> go (V2 B1 B0) <*> go (V2 B0 B1) <*> go (V2 B1 B1)
    where
    go i = branch i >>= \ b -> if b then iunfoldSparseM branch (leaf . (ib <$> i <*>)) else pure E
  {-# INLINABLE iunfoldSparseM #-}

  iunfoldSparse branch leaf = b (go (V2 B0 B0)) (go (V2 B1 B0)) (go (V2 B0 B1)) (go (V2 B1 B1))
    where
    go i = if branch i then iunfoldSparse branch (leaf . (ib <$> i <*>)) else E
  {-# INLINABLE iunfoldSparse #-}

instance Applicative (Quadtree 'S1) where
  pure = L

  E     <*> _ = E
  L   f <*> a = fmap f a

instance Applicative (Quadtree s) => Applicative (Quadtree ('S2x s)) where
  pure a = b (pure a) (pure a) (pure a) (pure a)

  E     <*> _     = E
  _     <*> E     = E
  B _ f1 f2 f3 f4 <*> B _ a1 a2 a3 a4 = b (f1 <*> a1) (f2 <*> a2) (f3 <*> a3) (f4 <*> a4)

b :: Quadtree s a -> Quadtree s a -> Quadtree s a -> Quadtree s a -> Quadtree ('S2x s) a
b lb rb lt rt
  | len > 0   = B len lb rb lt rt
  | otherwise = E
  where
  !len = length lb + length rb + length lt + length rt
{-# INLINABLE b #-}
