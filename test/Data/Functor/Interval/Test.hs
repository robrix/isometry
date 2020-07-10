{-# LANGUAGE OverloadedStrings #-}
module Data.Functor.Interval.Test
( tests
, interval
, superinterval
, delta
, nonZeroDelta
) where

import           Control.Monad (join)
import           Data.Functor.I
import           Data.Functor.Interval
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Test.Utils

tests :: [IO Bool]
tests = map checkParallel
  [ Group "point"
    [ prop "membership" $ do
      p <- pure <$> forAll gp
      member p (point p :: Interval I Int) === True

    ]
  , Group "isSubintervalOf"
    [ prop "reflexivity" $ do
      i <- forAll gi
      assert $ i `isSubintervalOf` i
    , prop "transitivity" $ do
      i1 <- forAll gi
      i2 <- forAll (superinterval delta i1)
      i3 <- forAll (superinterval delta i2)
      label $ (if i1 == i2 then "i1 = i2" else "i1 ⊂ i2") <> " ∧ " <> (if i2 == i3 then "i2 = i3" else "i2 ⊂ i3")
      assert (i1 `isSubintervalOf` i3)
    ]
  , Group "isProperSubintervalOf"
    [ prop "antireflexivity" $ do
      i <- forAll gi
      assert . not $ i `isProperSubintervalOf` i
    , prop "transitivity" $ do
      i1 <- forAll gi
      i2 <- forAll (superinterval nonZeroDelta i1)
      i3 <- forAll (superinterval nonZeroDelta i2)
      assert (i1 `isProperSubintervalOf` i3)
    ]
  , Group "union"
    [ prop "reflexivity" $ do
      i <- forAll gi
      i `union` i === i
    , prop "idempotence" $ do
      (i1, i2) <- forAll ((,) <$> gi <*> gi)
      let u = i1 `union` i2
      u `union` i1 === u
      u `union` i2 === u
    , prop "associativity" $ do
      (i1, i2, i3) <- forAll ((,,) <$> gi <*> gi <*> gi)
      (i1 `union` i2) `union` i3 === i1 `union` (i2 `union` i3)
    , prop "commutativity" $ do
      (i1, i2) <- forAll ((,) <$> gi <*> gi)
      i1 `union` i2 === (i2 `union` i1)
    ]
  , Group "interval"
    [ prop "validity" (forAll gi >>= assert . isValid)
    , ("coverage", verifiedTermination . withConfidence (10^(6 :: Int)) . property $ do
      i <- forAll gi
      cover 20 "point" (inf i == sup i)
      cover 20 "span" (inf i < sup i))
    ]
  , Group "superinterval"
    [ prop "validity" (forAll gi >>= forAll . superinterval delta >>= assert . isValid)
    , ("coverage", verifiedTermination . withConfidence (10^(6 :: Int)) . property $ do
      i <- forAll gi >>= forAll . superinterval delta
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

superinterval :: (MonadGen m, Num a) => m a -> Interval I a -> m (Interval I a)
superinterval delta i = do
  l <- delta
  r <- delta
  pure $! Interval (inf i - pure l) (sup i + pure r)

delta :: (MonadGen m, Num a) => m a
delta = Gen.choice [ pure 0, fromIntegral <$> Gen.int (Range.linear 0 10) ]

nonZeroDelta :: (MonadGen m, Num a) => m a
nonZeroDelta = (+ 1) <$> delta
