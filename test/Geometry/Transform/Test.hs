{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Geometry.Transform.Test
( tests
) where

import           Data.Coerce
import           Data.Functor.I
import           Geometry.Transform
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Linear.Exts hiding (identity)
import           Unit

prop_apply_identity = property $ do
  vec <- forAll $ v4 u
  identity `apply` vec === vec


prop_mkTranslation = property $ do
  v1 <- forAll $ v3 u
  let t = mkTranslation v1
  v2 <- forAll $ v4 u
  t `apply` v2 === ext v1 0 + v2
  inverse t `apply` (ext v1 0 + v2) === v2

prop_mkScale = property $ do
  v1 <- forAll $ v3 (U <$> pos)
  let t = mkScale (coerce v1)
  v2 <- forAll $ v4 u
  t `apply` v2 === ext v1 1 * v2
  inverse t `apply` (ext v1 1 * v2) === v2


coord ::  MonadGen m => m Rational
coord = Gen.realFrac_ (Range.linearFrac 0 100)

pos ::  MonadGen m => m Rational
pos = Gen.realFrac_ (Range.linearFrac 1 100)

u :: MonadGen m => m (U Rational)
u = U <$> coord

v :: MonadGen m => m (V Rational)
v = V <$> coord

v3 :: MonadGen m => m a -> m (V3 a)
v3 g = V3 <$> g <*> g <*> g

v4 :: (MonadGen m, Num a) => m a -> m (V4 a)
v4 g = (`ext` 1) <$> v3 g


newtype U a = U { getU :: a }
  deriving (Eq, Floating, Fractional, Num, Ord, Real, Show)
  deriving (Applicative, Functor, Unit I) via I

newtype V a = V { getV :: a }
  deriving (Eq, Floating, Fractional, Num, Ord, Real, Show)
  deriving (Applicative, Functor, Unit I) via I

newtype W a = W { getW :: a }
  deriving (Eq, Floating, Fractional, Num, Ord, Real, Show)
  deriving (Applicative, Functor, Unit I) via I


tests :: IO Bool
tests = checkParallel $$(discover)
