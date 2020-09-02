{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module Geometry.Plane.Test
( tests
) where

import           Geometry.Plane
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Linear.Metric
import           Linear.V2

prop_signedDistance_identity = property $ do
  v <- forAll $ v2 coord
  n <- forAll $ v2 (nonZero coord)
  signedDistance v n v === 0

prop_signedDistance_orthogonal = property $ do
  v <- forAll $ v2 coord
  n <- forAll $ v2 (nonZero coord)
  Near (signedDistance v n (v + perp n)) === 0

prop_signedDistance_unit = property $ do
  v <- forAll $ v2 coord
  n <- signorm <$> forAll (v2 (nonZero coord))
  Near (signedDistance v n (v + n)) === 1


coord :: MonadGen m => m Double
coord = Gen.realFrac_ (Range.linearFracFrom 0 (-100) 100)

nonZero :: (MonadGen m, Num a, Eq a) => m a -> m a
nonZero = Gen.filterT (/= 0)

v2 :: MonadGen m => m a -> m (V2 a)
v2 g = V2 <$> g <*> g


tests :: IO Bool
tests = checkParallel $$(discover)


roundToPlaces :: RealFrac a => Int -> a -> a
roundToPlaces p x = fromInteger (round (x * p')) / p'
  where
  p' = 10 ^ p


newtype Near a = Near a
  deriving (Num, Show)

instance RealFrac a => Eq (Near a) where
  Near a == Near b = roundToPlaces 5 a == roundToPlaces 5 b
