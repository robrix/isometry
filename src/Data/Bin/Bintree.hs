{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
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
