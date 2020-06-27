{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
module Data.Bin.Octree
( Octree(..)
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
  B :: !(Octree s a) -> !(Octree s a)
    -> !(Octree s a) -> !(Octree s a)
    -> !(Octree s a) -> !(Octree s a)
    -> !(Octree s a) -> !(Octree s a)
    -> Octree ('S2x s) a

deriving instance Foldable    (Octree s)
deriving instance Functor     (Octree s)
deriving instance Traversable (Octree s)

instance FoldableWithIndex (V3 (Index s)) (Octree s) where
  ifoldMap f = \case
    E   -> mempty
    L a -> f (pure IL) a
    B lbf rbf
      ltf rtf
      lbn rbn
      ltn rtn
      -> ifoldMap (f . (IB <$> V3 I0 I0 I0 <*>)) lbf <> ifoldMap (f . (IB <$> V3 I1 I0 I0 <*>)) rbf
      <> ifoldMap (f . (IB <$> V3 I0 I1 I0 <*>)) ltf <> ifoldMap (f . (IB <$> V3 I1 I1 I0 <*>)) rtf
      <> ifoldMap (f . (IB <$> V3 I0 I0 I1 <*>)) lbn <> ifoldMap (f . (IB <$> V3 I1 I0 I1 <*>)) rbn
      <> ifoldMap (f . (IB <$> V3 I0 I1 I1 <*>)) ltn <> ifoldMap (f . (IB <$> V3 I1 I1 I1 <*>)) rtn

instance SparseUnfoldableWithIndex V3 (Index 'S1) (Octree 'S1) where
  iunfoldSparseM _ leaf = L <$> leaf (pure IL)

instance SparseUnfoldableWithIndex V3 (Index s) (Octree s) => SparseUnfoldableWithIndex V3 (Index ('S2x s)) (Octree ('S2x s)) where
  iunfoldSparseM branch leaf = B
    <$> go (V3 I0 I0 I0) <*> go (V3 I1 I0 I0)
    <*> go (V3 I0 I1 I0) <*> go (V3 I1 I1 I0)
    <*> go (V3 I0 I0 I1) <*> go (V3 I1 I0 I1)
    <*> go (V3 I0 I1 I1) <*> go (V3 I1 I1 I1)
    where
    go i = branch i >>= \ b -> if b then iunfoldSparseM branch (leaf . (IB <$> i <*>)) else pure E
