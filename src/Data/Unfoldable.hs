{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Unfoldable
( SparseUnfoldableWithIndex(..)
) where

import Data.Coerce
import Data.Functor.Identity

-- | Unfolding of finite sparse structures with an index.
class SparseUnfoldableWithIndex b i t | t -> b i where
  iunfoldSparseM :: Monad m => (b -> m Bool) -> (i -> m a) -> m (t a)
  iunfoldSparse :: (b -> Bool) -> (i -> a) -> t a
  iunfoldSparse branch (leaf :: i -> a) = coerce (iunfoldSparseM :: (b -> Identity Bool) -> (i -> Identity a) -> Identity (t a)) branch leaf
  {-# INLINABLE iunfoldSparse #-}
