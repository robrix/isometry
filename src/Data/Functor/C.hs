{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Functor.C
( (:.:)(..)
) where

import Control.Applicative
import Control.Lens.Indexed

newtype (f :.: g) a = C { getC :: f (g a) }
  deriving (Eq, Foldable, Functor, Monoid, Ord, Semigroup, Show, Traversable)

infixr 7 :.:

instance (Applicative f, Applicative g) => Applicative (f :.: g) where
  pure = C . pure . pure
  {-# INLINE pure #-}

  C f <*> C a = C $ liftA2 (<*>) f a
  {-# INLINE (<*>) #-}

instance (FoldableWithIndex i f, FoldableWithIndex j g) => FoldableWithIndex (i, j) (f :.: g) where
  ifoldMap f = ifoldMap (ifoldMap . fmap f . (,)) . getC
