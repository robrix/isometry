{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeOperators #-}
module Data.Functor.C
( (:.:)(..)
) where

import Control.Applicative

newtype (f :.: g) a = C { getC :: f (g a) }
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

infixr 7 :.:

instance (Applicative f, Applicative g) => Applicative (f :.: g) where
  pure = C . pure . pure
  {-# INLINE pure #-}

  C f <*> C a = C $ liftA2 (<*>) f a
  {-# INLINE (<*>) #-}

instance Semigroup (f (g a)) => Semigroup ((f :.: g) a) where
  C f <> C g = C (f <> g)

instance Monoid (f (g a)) => Monoid ((f :.: g) a) where
  mempty = C mempty
