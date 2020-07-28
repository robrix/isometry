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


u :: MonadGen m => m (U Int)
u = U <$> Gen.int (Range.linear 0 100)

v :: MonadGen m => m (V Int)
v = V <$> Gen.int (Range.linear 0 100)

v4 :: (MonadGen m, Num a) => m a -> m (V4 a)
v4 g = V4 <$> g <*> g <*> g <*> pure 1


newtype U a = U { getU :: a }
  deriving (Eq, Floating, Fractional, Num, Ord, Real, Show)
  deriving (Applicative, Functor, Unit I) via I

newtype V a = V { getV :: a }
  deriving (Eq, Floating, Fractional, Num, Ord, Real, Show)
  deriving (Applicative, Functor, Unit I) via I


tests :: IO Bool
tests = checkParallel $$(discover)
