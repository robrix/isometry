{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Bin.Quadtree
( Quadtree(..)
) where

import Data.Bin.Shape

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
