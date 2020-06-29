{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
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
