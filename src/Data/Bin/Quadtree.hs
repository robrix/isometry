{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
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
