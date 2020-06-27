{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
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
