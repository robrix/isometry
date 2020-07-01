{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Unit.Multiple
( -- * Prefixes
  Mult(..)
  -- ** Submultiples
, Pico(..)
, Nano(..)
, Micro(..)
, Milli(..)
, Semi(..)
  -- ** Multiples
, Kilo(..)
, Mega(..)
, Giga(..)
, Tera(..)
) where

import Data.Functor.K
import Data.Proxy
import Foreign.Storable
import GHC.TypeLits
import GL.Type as GL
import GL.Uniform
import Linear.Conjugate
import Linear.Epsilon
import Linear.Metric
import Linear.Vector
import System.Random (Random)
import Unit

-- * Prefixes

newtype Mult (n :: Nat) (d :: Nat) (s :: Symbol) u a = Mult { getMult :: u a }
 deriving (Additive, Applicative, Column, Conjugate, Epsilon, Eq, Foldable, Floating, Fractional, Functor, Metric, Monad, Num, Ord, Random, Real, RealFloat, RealFrac, Row, Show, Storable, Traversable, GL.Type, Uniform)

instance (KnownNat n, KnownNat d, KnownSymbol s, Unit du u) => Unit du (Mult n d s u) where
  prj = prj . getMult

  factor = fromIntegral (natVal (Proxy @n)) / fromIntegral (natVal (Proxy @d))

  suffix = K ((symbolVal (Proxy @s) ++) . getK (suffix @_ @u))


-- ** Submultiples

newtype Pico u a = Pico { getPico :: u a }
  deriving (Additive, Applicative, Column, Conjugate, Epsilon, Eq, Foldable, Floating, Fractional, Functor, Metric, Monad, Num, Ord, Random, Real, RealFloat, RealFrac, Row, Storable, Traversable, GL.Type, Uniform)
  deriving (Unit d) via Mult 1 1_000_000_000_000 "p" u

deriving via Formatting (Pico u) a instance (Unit d u, RealFloat (u a)) => Show (Pico u a)

newtype Nano u a = Nano { getNano :: u a }
  deriving (Additive, Applicative, Column, Conjugate, Epsilon, Eq, Foldable, Floating, Fractional, Functor, Metric, Monad, Num, Ord, Random, Real, RealFloat, RealFrac, Row, Storable, Traversable, GL.Type, Uniform)
  deriving (Unit d) via Mult 1 1_000_000_000 "n" u

deriving via Formatting (Nano u) a instance (Unit d u, RealFloat (u a)) => Show (Nano u a)

newtype Micro u a = Micro { getMicro :: u a }
  deriving (Additive, Applicative, Column, Conjugate, Epsilon, Eq, Foldable, Floating, Fractional, Functor, Metric, Monad, Num, Ord, Random, Real, RealFloat, RealFrac, Row, Storable, Traversable, GL.Type, Uniform)
  deriving (Unit d) via Mult 1 1_000_000 "μ" u

deriving via Formatting (Micro u) a instance (Unit d u, RealFloat (u a)) => Show (Micro u a)

newtype Milli u a = Milli { getMilli :: u a }
  deriving (Additive, Applicative, Column, Conjugate, Epsilon, Eq, Foldable, Floating, Fractional, Functor, Metric, Monad, Num, Ord, Random, Real, RealFloat, RealFrac, Row, Storable, Traversable, GL.Type, Uniform)
  deriving (Unit d) via Mult 1 1_000 "m" u

deriving via Formatting (Milli u) a instance (Unit d u, RealFloat (u a)) => Show (Milli u a)

newtype Semi u a = Semi { getSemi :: u a }
  deriving (Additive, Applicative, Column, Conjugate, Epsilon, Eq, Foldable, Floating, Fractional, Functor, Metric, Monad, Num, Ord, Random, Real, RealFloat, RealFrac, Row, Storable, Traversable, GL.Type, Uniform)
  deriving (Unit d) via Mult 1 2 "semi" u

deriving via Formatting (Semi u) a instance (Unit d u, RealFloat (u a)) => Show (Semi u a)


-- ** Multiples

newtype Kilo u a = Kilo { getKilo :: u a }
  deriving (Additive, Applicative, Column, Conjugate, Epsilon, Eq, Foldable, Floating, Fractional, Functor, Metric, Monad, Num, Ord, Random, Real, RealFloat, RealFrac, Row, Storable, Traversable, GL.Type, Uniform)
  deriving (Unit d) via Mult 1_000 1 "k" u

deriving via Formatting (Kilo u) a instance (Unit d u, RealFloat (u a)) => Show (Kilo u a)

newtype Mega u a = Mega { getMega :: u a }
  deriving (Additive, Applicative, Column, Conjugate, Epsilon, Eq, Foldable, Floating, Fractional, Functor, Metric, Monad, Num, Ord, Random, Real, RealFloat, RealFrac, Row, Storable, Traversable, GL.Type, Uniform)
  deriving (Unit d) via Mult 1_000_000 1 "M" u

deriving via Formatting (Mega u) a instance (Unit d u, RealFloat (u a)) => Show (Mega u a)

newtype Giga u a = Giga { getGiga :: u a }
  deriving (Additive, Applicative, Column, Conjugate, Epsilon, Eq, Foldable, Floating, Fractional, Functor, Metric, Monad, Num, Ord, Random, Real, RealFloat, RealFrac, Row, Storable, Traversable, GL.Type, Uniform)
  deriving (Unit d) via Mult 1_000_000_000 1 "G" u

deriving via Formatting (Giga u) a instance (Unit d u, RealFloat (u a)) => Show (Giga u a)

newtype Tera u a = Tera { getTera :: u a }
  deriving (Additive, Applicative, Column, Conjugate, Epsilon, Eq, Foldable, Floating, Fractional, Functor, Metric, Monad, Num, Ord, Random, Real, RealFloat, RealFrac, Row, Storable, Traversable, GL.Type, Uniform)
  deriving (Unit d) via Mult 1_000_000_000_000 1 "T" u

deriving via Formatting (Tera u) a instance (Unit d u, RealFloat (u a)) => Show (Tera u a)
