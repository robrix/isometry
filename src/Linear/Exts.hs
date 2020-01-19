{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module Linear.Exts
( translated
, orient
, face
, easeInOutCubic
, reject
, direction
, angleOf
, angleTo
, isFacing
, toAxisAngle
, cartesian2
, polar2
, Ext(..)
, module Linear.Epsilon
, module Linear.Matrix
, module Linear.Metric
, module Linear.Quaternion
, module Linear.V1
, module Linear.V2
, module Linear.V3
, module Linear.V4
, module Linear.Vector
) where

import Data.Coerce
import Data.Functor.I
import Data.Functor.Interval
import Linear.Epsilon
import Linear.Matrix hiding (Trace(..))
import Linear.Metric
import Linear.Quaternion
import Linear.V1
import Linear.V2 hiding (angle)
import Linear.V3
import Linear.V4
import Linear.Vector

translated :: Num a => V2 a -> M33 a
translated (V2 tx ty) = V3
  (V3 1 0 tx)
  (V3 0 1 ty)
  (V3 0 0 1)


orient :: (Epsilon a, RealFloat a) => a -> a -> a -> Quaternion a
orient alpha beta gamma
  = axisAngle (unit _z) alpha
  * axisAngle (unit _x) beta
  * axisAngle (unit _z) gamma


-- | Compute a rotation turning to face a desired angle with a given maximum angular thrust.
face
  :: (Epsilon a, RealFloat a)
  => a            -- ^ Angular thrust. (Speed of rotation.)
  -> a            -- ^ Desired angle.
  -> Quaternion a -- ^ Current rotation.
  -> Quaternion a -- ^ Resulting rotation.
face angular angle rotation
  | nearZero delta = coerce proposed
  | otherwise      = slerp rotation (coerce proposed) (min 1 (angular / getI delta)) where
  proposed = axisAngle (unit _z) (I angle)
  delta = abs (wrap (Interval (-pi) pi) (I (snd (toAxisAngle rotation)) - I angle))


easeInOutCubic :: Double -> Double
easeInOutCubic t
  | t < 0.5   = 4 * t ** 3
  | otherwise = (t - 1) * (2 * t - 2) ** 2 + 1


reject :: (Metric v, Fractional a) => v a -> v a -> v a
reject a b = a ^-^ project a b


-- | The unit vector in the direction of another vector.
direction :: (Metric v, Epsilon a, Floating a) => v a -> v a -> v a
direction a b = normalize (a ^-^ b)


-- | The angle of a vector.
angleOf :: RealFloat a => V2 a -> a
angleOf (V2 x y) = atan2 y x -- FIXME: this should take input units and return dimensionless units

-- | The angle from the first vector to the second.
angleTo :: RealFloat a => V2 a -> V2 a -> a
angleTo v1 v2 = angleOf (v2 - v1) -- FIXME: this should take input units and return dimensionless units


isFacing :: (Real a, Floating a) => a -> Quaternion a -> a -> Bool
isFacing epsilon rotation target = abs (getI (wrap (Interval (-pi) pi) (I (snd (toAxisAngle rotation)))) - target) < epsilon


-- | Compute the axis/angle of a rotation represented as a unit quaternion.
--
-- NB: Assumes unit magnitude. The axis is undefined for 0-rotations.
toAxisAngle :: (Floating a, Ord a) => Quaternion a -> (V3 a, a)
toAxisAngle (Quaternion qw qv) = (v, phi) where
  v   = sign *^ qv ^/ sqrt (1 - qw ^ (2 :: Int))
  phi = sign * 2 * acos qw
  sign | qv >= 0   =  1
       | otherwise = -1


cartesian2 :: Floating a => a -> a -> V2 a
cartesian2 phi r = V2 (r * cos phi) (r * sin phi)

polar2 :: RealFloat a => V2 a -> (a, a)
polar2 v = (angleOf v, norm v)


-- | Extensions of a vector with an extra dimension.
class Ext v a v' | v v' -> a, v a -> v', v' a -> v where
  ext :: v -> a -> v'

instance Ext (V1 a) a (V2 a) where
  ext (V1 x) = V2 x

instance Ext (V2 a) a (V3 a) where
  ext (V2 x y) = V3 x y

instance Ext a (V2 a) (V3 a) where
  ext x (V2 y z) = V3 x y z

instance Ext (V2 a) (V2 a) (V4 a) where
  ext (V2 x y) (V2 z w) = V4 x y z w

instance Ext a (V3 a) (V4 a) where
  ext x (V3 y z w) = V4 x y z w

instance Ext (V3 a) a (V4 a) where
  ext (V3 x y z) = V4 x y z
