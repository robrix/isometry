{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
module Data.Bin.Octree
( Octree(..)
) where

import Control.Lens.Indexed
import Data.Bin.Index
import Data.Bin.Oct hiding (Octree)
import Data.Bin.Shape
import Data.Bin.Tree (SparseUnfoldableWithIndex(..), UnfoldableWithIndex(..))
import Data.Monoid (Sum(..))
import Linear.V3

data Octree s a where
  E :: Octree s a
  L :: !a -> Octree 'S1 a
  B :: {-# UNPACK #-} !Int
    -> {-# UNPACK #-} !(Oct (Octree s a))
    -> Octree ('S2x s) a

instance Foldable (Octree s) where
  foldMap (f :: a -> m) = go
    where
    go :: Octree s' a -> m
    go = \case
      E     -> mempty
      L   a -> f a
      B _ b -> foldMap go b

  length = \case
    E     -> 0
    L   _ -> 1
    B l _ -> l

deriving instance Functor     (Octree s)
deriving instance Traversable (Octree s)

instance FoldableWithIndex (V3 (Index s)) (Octree s) where
  ifoldMap f = \case
    E     -> mempty
    L   a -> f (pure IL) a
    B _ b -> ifoldMap (\ i -> ifoldMap (f . (IB <$> i <*>))) b

instance SparseUnfoldableWithIndex V3 (Index 'S1) (Octree 'S1) where
  iunfoldSparseM _ leaf = L <$> leaf (pure IL)

instance SparseUnfoldableWithIndex V3 (Index s) (Octree s) => SparseUnfoldableWithIndex V3 (Index ('S2x s)) (Octree ('S2x s)) where
  iunfoldSparseM branch leaf = makeB <$> iunfoldA go
    where
    go i = branch i >>= \ b -> if b then iunfoldSparseM branch (leaf . (IB <$> i <*>)) else pure E

makeB :: Oct (Octree s a) -> Octree ('S2x s) a
makeB o = B (getSum (foldMap (Sum . length) o)) o
