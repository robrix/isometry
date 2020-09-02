{-# LANGUAGE TemplateHaskell #-}
module Geometry.Plane.Test
( tests
) where

import           Geometry.Plane
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Linear.V2

prop_signedDistance_identity = property $ do
  v <- forAll $ v2 coord
  n <- forAll $ v2 (nonZero coord)
  signedDistance v n v === 0


coord :: MonadGen m => m Rational
coord = Gen.realFrac_ (Range.linearFracFrom 0 (-100) 100)

nonZero :: (MonadGen m, Num a, Eq a) => m a -> m a
nonZero = Gen.filterT (/= 0)

v2 :: MonadGen m => m a -> m (V2 a)
v2 g = V2 <$> g <*> g


tests :: IO Bool
tests = checkParallel $$(discover)
