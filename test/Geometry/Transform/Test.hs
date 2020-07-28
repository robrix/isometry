{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Geometry.Transform.Test
( tests
) where

import           Data.Functor.I
import           Geometry.Transform
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Linear.Exts hiding (identity)
import           Unit

prop_apply_identity = property $ do
  vec <- forAll (v4 u)
  identity `apply` vec === vec


prop_apply'_identity = property $ do
  vec <- forAll (v4 u)
  identity `apply'` vec === vec


translation :: MonadGen m => m (Transform V4 Int U U)
translation = mkTranslation <$> v3 u

int ::  MonadGen m => m Int
int = Gen.int (Range.linear 0 100)

u :: MonadGen m => m (U Int)
u = U <$> int

v :: MonadGen m => m (V Int)
v = V <$> int

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


tests :: IO Bool
tests = checkParallel $$(discover)
