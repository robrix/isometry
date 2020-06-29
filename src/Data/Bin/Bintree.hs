{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
module Data.Bin.Bintree
( Bintree(..)
) where

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
