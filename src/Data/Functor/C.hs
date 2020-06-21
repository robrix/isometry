{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
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
import Linear.Vector

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

instance (FoldableWithIndex (E f) f, FoldableWithIndex (E g) g) => FoldableWithIndex (E (f :.: g)) (f :.: g) where
  ifoldMap f = ifoldMap (\ i -> ifoldMap (\ j -> f (E (compose_.el i.el j)))) . getC
  {-# INLINABLE ifoldMap #-}

instance (FunctorWithIndex (E f) f, FunctorWithIndex (E g) g) => FunctorWithIndex (E (f :.: g)) (f :.: g) where
  imap f = C . imap (\ i -> imap (\ j -> f (E (compose_.el i.el j)))) . getC
  {-# INLINABLE imap #-}

instance (TraversableWithIndex (E f) f, TraversableWithIndex (E g) g) => TraversableWithIndex (E (f :.: g)) (f :.: g) where
  itraverse f = fmap C . itraverse (\ i -> itraverse (\ j -> f (E (compose_.el i.el j)))) . getC
  {-# INLINABLE itraverse #-}
