{-# LANGUAGE OverloadedStrings #-}
module Data.Functor.Interval.Test
( tests
, interval
) where

import           Control.Monad (join)
import           Data.Functor.I
import           Data.Functor.Interval
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

tests :: [IO Bool]
tests = map checkParallel
  [ Group "point"
    [ ("membership", property $ do
      p <- pure <$> forAll gp
      member p (point p :: Interval I Int) === True
      )
    ]
  , Group "union"
    [ ("reflexivity", property $ do
      i <- forAll gi
      i `union` i === i)
    ]
  , Group "interval"
    [ ("validity", property (forAll gi >>= assert . isValid))
    , ("coverage", verifiedTermination . withConfidence (10^(6 :: Int)) . property $ do
      i <- forAll gi
      cover 20 "point" (inf i == sup i)
      cover 20 "span" (inf i < sup i))
    ]
  ]
  where
  gp = Gen.int (Range.linear 0 100)
  gi = interval gp


interval :: (MonadGen m, Num a) => m a -> m (Interval I a)
interval p = Gen.choice
  [ join (...) <$> p
  , mk <$> p <*> p
  ]
  where
  mk a b = a ... a + b + 1
