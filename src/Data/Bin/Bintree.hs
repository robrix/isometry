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
module Data.Bin.Bintree
( Bintree(..)
) where

import Control.Lens.Indexed
import Data.Bin.Bit
import Data.Bin.Index
import Data.Bin.Shape
import Data.Bin.Tree (SparseUnfoldableWithIndex(..))

data Bintree s a where
  E :: Bintree s a
  L :: !a -> Bintree 'S1 a
  B :: {-# UNPACK #-} !Int
    -> !(Bintree s a) -> !(Bintree s a)
    -> Bintree ('S2x s) a

instance Foldable (Bintree s) where
  foldMap (f :: a -> m) = go
    where
    go :: Bintree s' a -> m
    go = \case
      E       -> mempty
      L a     -> f a
      B _ l r -> go l <> go r
  {-# INLINABLE foldMap #-}

  length = \case
    E       -> 0
    L _     -> 1
    B l _ _ -> l
  {-# INLINABLE length #-}

deriving instance Functor     (Bintree s)
deriving instance Traversable (Bintree s)

instance FoldableWithIndex (Index s) (Bintree s) where
  ifoldMap f = \case
    E     -> mempty
    L   a -> f il a
    B _ l r
      -> go B0 l <> go B1 r
      where
      go b = ifoldMap (f . ib b)
  {-# INLINABLE ifoldMap #-}

instance SparseUnfoldableWithIndex Bit (Index 'S1) (Bintree 'S1) where
  iunfoldSparseM _ leaf = L <$> leaf il
  {-# INLINABLE iunfoldSparseM #-}

  iunfoldSparse _ leaf = L (leaf il)
  {-# INLINABLE iunfoldSparse #-}

instance SparseUnfoldableWithIndex Bit (Index s) (Bintree s) => SparseUnfoldableWithIndex Bit (Index ('S2x s)) (Bintree ('S2x s)) where
  iunfoldSparseM branch leaf = b <$> go B0 <*> go B1
    where
    go i = branch i >>= \ b -> if b then iunfoldSparseM branch (leaf . ib i) else pure E
  {-# INLINABLE iunfoldSparseM #-}

  iunfoldSparse branch leaf = b (go B0) (go B1)
    where
    go i = if branch i then iunfoldSparse branch (leaf . ib i) else E
  {-# INLINABLE iunfoldSparse #-}

instance Applicative (Bintree 'S1) where
  pure = L

  E     <*> _ = E
  L   f <*> a = fmap f a

instance Applicative (Bintree s) => Applicative (Bintree ('S2x s)) where
  pure a = b (pure a) (pure a)

  E     <*> _     = E
  _     <*> E     = E
  B _ f1 f2 <*> B _ a1 a2 = b (f1 <*> a1) (f2 <*> a2)

b :: Bintree s a -> Bintree s a -> Bintree ('S2x s) a
b l r
  | len > 0   = B len l r
  | otherwise = E
  where
  !len = length l + length r
{-# INLINABLE b #-}
