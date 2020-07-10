{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Functor.Interval
( Interval(..)
, inf_
, sup_
, (...)
, point
, dimensionwise
, liftI
, size
, toUnit
, fromUnit
, range
, ranges
, wrap
, imap
, member
, isValid
, isPoint
, isSubintervalOf
, isSuperintervalOf
, isProperSubintervalOf
, isProperSuperintervalOf
, before
, after
, uniformI
, Union(..)
, union
, Intersection(..)
, intersection
, intersects
  -- * Comparisons
, liftRelation
, lt
, lte
, gt
, gte
) where

import           Control.Applicative (liftA2)
import           Control.Effect.Random
import           Control.Lens (Lens', lens)
import           Control.Monad.Trans.Class
import           Data.Coerce (coerce)
import           Data.Fixed (mod')
import           Data.Functor.I
import           GHC.Generics (Generic)
import qualified System.Random as R

data Interval f a = Interval
  { inf :: !(f a)
  , sup :: !(f a)
  }
  deriving (Eq, Foldable, Functor, Generic, Ord, Traversable)

instance Show (f a) => Show (Interval f a) where
  showsPrec p i = showParen (p > 3) $ showsPrec 4 (inf i) . showString "..." . showsPrec 4 (sup i)

instance Applicative f => Applicative (Interval f) where
  pure = point . pure
  f <*> a = Interval (inf f <*> inf a) (sup f <*> sup a)
  a *> b = Interval (inf a *> inf b) (sup a *> sup b)
  a <* b = Interval (inf a <* inf b) (sup a <* sup b)
  liftA2 f a b = Interval (liftA2 f (inf a) (inf b)) (liftA2 f (sup a) (sup b))

instance Monad f => Monad (Interval f) where
  m >>= f = Interval (inf m >>= inf . f) (sup m >>= sup . f)

instance MonadTrans Interval where
  lift = point

instance (Applicative f, Num a) => Num (Interval f a) where
  (+) = liftA2 (+)
  {-# INLINE (+) #-}
  (*) = liftA2 (*)
  {-# INLINE (*) #-}
  (-) = liftA2 (-)
  {-# INLINE (-) #-}
  abs = fmap abs
  {-# INLINE abs #-}
  signum = fmap signum
  {-# INLINE signum #-}
  negate = fmap negate
  {-# INLINE negate #-}
  fromInteger = pure . fromInteger
  {-# INLINE fromInteger #-}

instance (Applicative f, Fractional a) => Fractional (Interval f a) where
  recip = fmap recip
  {-# INLINE recip #-}
  (/) = liftA2 (/)
  {-# INLINE (/) #-}
  fromRational = pure . fromRational
  {-# INLINE fromRational #-}

instance (Applicative f, Floating a) => Floating (Interval f a) where
  pi = pure pi
  {-# INLINE pi #-}
  exp = fmap exp
  {-# INLINE exp #-}
  sqrt = fmap sqrt
  {-# INLINE sqrt #-}
  log = fmap log
  {-# INLINE log #-}
  (**) = liftA2 (**)
  {-# INLINE (**) #-}
  logBase = liftA2 logBase
  {-# INLINE logBase #-}
  sin = fmap sin
  {-# INLINE sin #-}
  tan = fmap tan
  {-# INLINE tan #-}
  cos = fmap cos
  {-# INLINE cos #-}
  asin = fmap asin
  {-# INLINE asin #-}
  atan = fmap atan
  {-# INLINE atan #-}
  acos = fmap acos
  {-# INLINE acos #-}
  sinh = fmap sinh
  {-# INLINE sinh #-}
  tanh = fmap tanh
  {-# INLINE tanh #-}
  cosh = fmap cosh
  {-# INLINE cosh #-}
  asinh = fmap asinh
  {-# INLINE asinh #-}
  atanh = fmap atanh
  {-# INLINE atanh #-}
  acosh = fmap acosh
  {-# INLINE acosh #-}

instance (Applicative f, Ord a) => Semigroup (Interval f a) where
  (<>) = union


inf_ :: Lens' (Interval f a) (f a)
inf_ = lens inf $ \ i inf -> i{ inf }

sup_ :: Lens' (Interval f a) (f a)
sup_ = lens sup $ \ i sup -> i{ sup }


(...) :: Applicative f => a -> a -> Interval f a
inf...sup = Interval (pure inf) (pure sup)

infix 3 ...

point :: f a -> Interval f a
point p = Interval p p

dimensionwise :: Applicative f => (Interval I a -> b) -> Interval f a -> f b
dimensionwise f = liftI (fmap f . (...))

liftI :: Applicative f => (a -> a -> b) -> Interval f a -> f b
liftI f i = liftA2 f (inf i) (sup i)

size :: (Applicative f, Num a) => Interval f a -> f a
size = liftI (flip (-))

toUnit, fromUnit :: (Applicative f, Fractional a) => Interval f a -> f a -> f a
toUnit   i x = dimensionwise (\ i x -> getI ((I x - inf  i) / size i)) i <*> x
fromUnit i x = dimensionwise (\ i x -> getI  (I x * size i  + inf  i)) i <*> x


range :: Enum (f a) => Interval f a -> [f a]
range = enumFromTo . inf <*> sup

ranges :: (Applicative f, Enum a) => Interval f a -> f [a]
ranges = liftI enumFromTo


wrap :: (Applicative f, Real a) => Interval f a -> f a -> f a
wrap i x = dimensionwise (\ i x -> getI (((I x + sup i) `mod'` size i) + inf i)) i <*> x


imap :: (f a -> g b) -> Interval f a -> Interval g b
imap f i = Interval (f (inf i)) (f (sup i))


member :: (Applicative f, Foldable f, Ord a) => f a -> Interval f a -> Bool
member = isSubintervalOf . point


isValid :: (Applicative f, Foldable f, Ord a) => Interval f a -> Bool
isValid i = inf i `lte` sup i

isPoint :: (Applicative f, Foldable f, Eq a) => Interval f a -> Bool
isPoint = and . liftI (==)


isSubintervalOf :: (Applicative f, Foldable f, Ord a) => Interval f a -> Interval f a -> Bool
isSubintervalOf a b = inf a `gte` inf b && sup a `lte` sup b

isSuperintervalOf :: (Applicative f, Foldable f, Ord a) => Interval f a -> Interval f a -> Bool
isSuperintervalOf = flip isSubintervalOf

isProperSubintervalOf :: (Applicative f, Foldable f, Ord a) => Interval f a -> Interval f a -> Bool
isProperSubintervalOf a b = isSubintervalOf a b && or (liftA2 (/=) a b)

isProperSuperintervalOf :: (Applicative f, Foldable f, Ord a) => Interval f a -> Interval f a -> Bool
isProperSuperintervalOf a b = isSuperintervalOf a b && or (liftA2 (/=) a b)


before, after :: (Applicative f, Foldable f, Ord a) => Interval f a -> Interval f a -> Bool
before a b = inf a `lte` sup b
after  a b = sup a `lt`  sup b


uniformI :: (R.Random a, Applicative f, Traversable f, Has Random sig m) => Interval f a -> m (f a)
uniformI i = traverse uniformR (liftI (,) i)


newtype Union f a = Union { getUnion :: Interval f a }
  deriving (Applicative, Eq, Foldable, Functor, Monad, Ord, Show, Traversable)

instance (Applicative f, Ord a) => Semigroup (Union f a) where
  Union i1 <> Union i2 = Union ((min...max) <*> i1 <*> i2)

union :: forall f a . (Applicative f, Ord a) => Interval f a -> Interval f a -> Interval f a
union = coerce ((<>) :: Union f a -> Union f a -> Union f a)


newtype Intersection f a = Intersection { getIntersection :: Interval f a }
  deriving (Applicative, Eq, Foldable, Functor, Monad, Ord, Show, Traversable)

instance (Applicative f, Ord a) => Semigroup (Intersection f a) where
  Intersection i1 <> Intersection i2 = Intersection ((max...min) <*> i1 <*> i2)

intersection :: forall f a . (Applicative f, Ord a) => Interval f a -> Interval f a -> Interval f a
intersection = coerce ((<>) :: Intersection f a -> Intersection f a -> Intersection f a)


intersects :: (Applicative f, Foldable f, Ord a) => Interval f a -> Interval f a -> Bool
intersects a b = isValid (intersection a b)


-- Comparisons

liftRelation :: (Applicative f, Foldable f) => (a -> b -> Bool) -> f a -> f b -> Bool
liftRelation rel a b = and (liftA2 rel a b)

infix 4 `lt`, `lte`, `gt`, `gte`

lt :: (Applicative f, Foldable f, Ord a) => f a -> f a -> Bool
lt = liftRelation (<)

lte :: (Applicative f, Foldable f, Ord a) => f a -> f a -> Bool
lte = liftRelation (<=)

gt :: (Applicative f, Foldable f, Ord a) => f a -> f a -> Bool
gt = liftRelation (>)

gte :: (Applicative f, Foldable f, Ord a) => f a -> f a -> Bool
gte = liftRelation (>=)
