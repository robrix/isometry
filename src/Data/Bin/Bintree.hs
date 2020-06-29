{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
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
