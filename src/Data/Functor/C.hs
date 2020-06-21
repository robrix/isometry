{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Functor.C
( (:.:)(..)
, compose_
) where

import Control.Applicative
import Control.Lens (Iso', iso)
import Control.Lens.Indexed

newtype (f :.: g) a = C { getC :: f (g a) }
  deriving (Eq, Foldable, Functor, Monoid, Ord, Semigroup, Show, Traversable)

infixr 7 :.:

compose_ :: Iso' ((f :.: g) a) (f (g a))
compose_ = iso getC C

instance (Applicative f, Applicative g) => Applicative (f :.: g) where
  pure = C . pure . pure
  {-# INLINE pure #-}

  C f <*> C a = C $ liftA2 (<*>) f a
  {-# INLINE (<*>) #-}

instance (FoldableWithIndex i f, FoldableWithIndex j g) => FoldableWithIndex (i, j) (f :.: g) where
  ifoldMap f = ifoldMap (ifoldMap . fmap f . (,)) . getC
  {-# INLINABLE ifoldMap #-}

instance (FunctorWithIndex i f, FunctorWithIndex j g) => FunctorWithIndex (i, j) (f :.: g) where
  imap f = C . imap (imap . fmap f . (,)) . getC
  {-# INLINABLE imap #-}

instance (TraversableWithIndex i f, TraversableWithIndex j g) => TraversableWithIndex (i, j) (f :.: g) where
  itraverse f = fmap C . itraverse (itraverse . fmap f . (,)) . getC
  {-# INLINABLE itraverse #-}
