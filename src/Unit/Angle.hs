{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Unit.Angle
( Angle
, Radians(..)
, radians
, Degrees(..)
, module Unit
, module Unit.Algebra
, module Unit.Multiple
) where

import Data.Functor.I
import Data.Functor.Interval
import Data.Functor.K
import Foreign.Storable
import GL.Type as GL
import GL.Uniform
import Linear.Conjugate
import Linear.Epsilon
import Linear.Metric
import Linear.Vector
import System.Random (Random)
import Unit
import Unit.Algebra
import Unit.Multiple

type Angle = I


newtype Radians a = Radians { getRadians :: a }
  deriving (Column, Conjugate, Epsilon, Enum, Eq, Foldable, Floating, Fractional, Functor, Integral, Num, Ord, Random, Real, RealFloat, RealFrac, Row, Show, Storable, Traversable, GL.Type, Uniform)
  deriving (Additive, Applicative, Metric, Monad) via I

instance Unit I Radians where
  suffix = K ("rad"++)

radians :: Floating a => Interval Radians a
radians = -pi...pi


newtype Degrees a = Degrees { getDegrees :: a }
  deriving (Column, Conjugate, Epsilon, Enum, Eq, Foldable, Floating, Fractional, Functor, Integral, Num, Ord, Random, Real, RealFloat, RealFrac, Row, Show, Storable, Traversable, GL.Type, Uniform)
  deriving (Additive, Applicative, Metric, Monad) via I

instance Unit I Degrees where
  suffix = K ('°':)
  factor = K (pi/180)
