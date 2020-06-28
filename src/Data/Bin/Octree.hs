{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Bin.Octree
( Octree
) where

import Control.Lens.Indexed
import Data.Bin.Bit
import Data.Bin.Index
import Data.Bin.Shape
import Data.Bin.Tree (SparseUnfoldableWithIndex(..))
import Linear.V3

data Octree s a where
  E :: Octree s a
  L :: !a -> Octree 'S1 a
  B :: {-# UNPACK #-} !Int
    -> {-# UNPACK #-} !(Oct s a)
    -> Octree ('S2x s) a

instance Foldable (Octree s) where
  foldMap (f :: a -> m) = go
    where
    go :: Octree s' a -> m
    go = \case
      E     -> mempty
      L   a -> f a
      B _ b -> foldMap f b
  {-# INLINABLE foldMap #-}

  length = \case
    E     -> 0
    L   _ -> 1
    B l _ -> l
  {-# INLINABLE length #-}

deriving instance Functor     (Octree s)
deriving instance Traversable (Octree s)

instance FoldableWithIndex (V3 (Index s)) (Octree s) where
  ifoldMap f = \case
    E     -> mempty
    L   a -> f (pure il) a
    B _ b -> ifoldMap f b
  {-# INLINABLE ifoldMap #-}

instance SparseUnfoldableWithIndex V3 (Index 'S1) (Octree 'S1) where
  iunfoldSparseM _ leaf = L <$> leaf (pure il)
  {-# INLINABLE iunfoldSparseM #-}

instance SparseUnfoldableWithIndex V3 (Index s) (Octree s) => SparseUnfoldableWithIndex V3 (Index ('S2x s)) (Octree ('S2x s)) where
  iunfoldSparseM branch leaf = makeB <$> iunfoldSparseM branch leaf
  {-# INLINABLE iunfoldSparseM #-}

instance Applicative (Octree 'S1) where
  pure = L

  E     <*> _ = E
  L   f <*> a = fmap f a

instance Applicative (Octree s) => Applicative (Octree ('S2x s)) where
  pure = makeB . pure

  E     <*> _     = E
  _     <*> E     = E
  B _ f <*> B _ a = makeB (f <*> a)

makeB :: Oct s a -> Octree ('S2x s) a
makeB o = B (length o) o
{-# INLINABLE makeB #-}


data Oct s a = Oct
  { lbf, rbf, ltf, rtf, lbn, rbn, ltn, rtn :: Octree s a
  }
  deriving (Functor, Traversable)

instance Foldable (Oct s) where
  foldMap f Oct{ lbf, rbf, ltf, rtf, lbn, rbn, ltn, rtn }
    =  foldMap f lbf <> foldMap f rbf
    <> foldMap f ltf <> foldMap f rtf
    <> foldMap f lbn <> foldMap f rbn
    <> foldMap f ltn <> foldMap f rtn
  {-# INLINABLE foldMap #-}

  length Oct{ lbf, rbf, ltf, rtf, lbn, rbn, ltn, rtn } = length lbf + length rbf + length ltf + length rtf + length lbn + length rbn + length ltn + length rtn
  {-# INLINABLE length #-}

instance FoldableWithIndex (V3 (Index ('S2x s))) (Oct s) where
  ifoldMap f Oct{ lbf, rbf, ltf, rtf, lbn, rbn, ltn, rtn }
    =  go B0 B0 B0 lbf <> go B1 B0 B0 rbf
    <> go B0 B1 B0 ltf <> go B1 B1 B0 rtf
    <> go B0 B0 B1 lbn <> go B1 B0 B1 rbn
    <> go B0 B1 B1 ltn <> go B1 B1 B1 rtn
    where
    go x y z = ifoldMap (f . (ib <$> V3 x y z <*>))

instance SparseUnfoldableWithIndex V3 (Index s) (Octree s) => SparseUnfoldableWithIndex V3 (Index ('S2x s)) (Oct s) where
  iunfoldSparseM branch leaf = Oct
    <$> go (V3 B0 B0 B0) <*> go (V3 B1 B0 B0)
    <*> go (V3 B0 B1 B0) <*> go (V3 B1 B1 B0)
    <*> go (V3 B0 B0 B1) <*> go (V3 B1 B0 B1)
    <*> go (V3 B0 B1 B1) <*> go (V3 B1 B1 B1)
    where
    go i = branch i >>= \ b -> if b then iunfoldSparseM branch (leaf . (ib <$> i <*>)) else pure E

instance Applicative (Octree s) => Applicative (Oct s) where
  pure a = Oct (pure a) (pure a) (pure a) (pure a) (pure a) (pure a) (pure a) (pure a)

  Oct f1 f2 f3 f4 f5 f6 f7 f8 <*> Oct a1 a2 a3 a4 a5 a6 a7 a8 = Oct (f1 <*> a1) (f2 <*> a2) (f3 <*> a3) (f4 <*> a4) (f5 <*> a5) (f6 <*> a6) (f7 <*> a7) (f8 <*> a8)
