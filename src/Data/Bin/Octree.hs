{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
module Data.Bin.Octree
( Octree(..)
) where

import Data.Bin.Shape

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
