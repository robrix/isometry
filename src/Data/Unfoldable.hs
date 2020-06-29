{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Unfoldable
( SparseUnfoldableWithIndex(..)
, tetra
) where

import Data.Bin.Bit
import Data.Bits (xor)
import Data.Coerce
import Data.Foldable (foldl')
import Data.Functor.Identity

-- | Unfolding of finite sparse structures with an index.
class SparseUnfoldableWithIndex b i t | t -> b i where
  iunfoldSparseM :: Monad m => (b -> m Bool) -> (i -> m a) -> m (t a)
  iunfoldSparse :: (b -> Bool) -> (i -> a) -> t a
  iunfoldSparse branch (leaf :: i -> a) = coerce (iunfoldSparseM :: (b -> Identity Bool) -> (i -> Identity a) -> Identity (t a)) branch leaf
  {-# INLINABLE iunfoldSparse #-}

tetra :: (Foldable v, SparseUnfoldableWithIndex (v Bit) i t) => (i -> a) -> t a
tetra = iunfoldSparse (fromBit . foldl' xor B0)
{-# INLINABLE tetra #-}
