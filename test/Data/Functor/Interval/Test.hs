{-# LANGUAGE OverloadedStrings #-}
module Data.Functor.Interval.Test
( tests
, interval
, superinterval
, properSuperinterval
, delta
, nonZeroDelta
) where

import           Control.Lens ((&), (+~), (-~))
import           Control.Monad (join)
import           Data.Functor.I
import           Data.Functor.Interval
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

tests :: [IO Bool]
tests = map checkParallel
  [ Group "point"
    [ (,) "membership" $ property $ do
      p <- pure <$> forAll gp
      member p (point p :: Interval I Int) === True
    ]

  , Group "isSubintervalOf"
    [ (,) "reflexivity" $ property $ do
      i <- forAll gi
      assert $ i `isSubintervalOf` i
    , (,) "transitivity" $ property $ do
      i1 <- forAll gi
      i2 <- forAll (superinterval i1)
      i3 <- forAll (superinterval i2)
      label $ (if i1 == i2 then "i1 = i2" else "i1 ⊂ i2") <> " ∧ " <> (if i2 == i3 then "i2 = i3" else "i2 ⊂ i3")
      assert (i1 `isSubintervalOf` i3)
    , (,) "offset" $ property $ do
      i <- forAll gi
      d <- forAll nonZeroDelta
      assert . not $ i `isSubintervalOf` imap (+ d) i
      assert . not $ imap (+ d) i `isSubintervalOf` i
    ]

  , Group "isSuperintervalOf"
    [ (,) "reflexivity" $ property $ do
      i <- forAll gi
      assert $ i `isSuperintervalOf` i
    , (,) "transitivity" $ property $ do
      i1 <- forAll gi
      i2 <- forAll (superinterval i1)
      i3 <- forAll (superinterval i2)
      label $ (if i1 == i2 then "i1 = i2" else "i1 ⊂ i2") <> " ∧ " <> (if i2 == i3 then "i2 = i3" else "i2 ⊂ i3")
      assert (i3 `isSuperintervalOf` i1)
    , (,) "offset" $ property $ do
      i <- forAll gi
      d <- forAll nonZeroDelta
      assert . not $ i `isSuperintervalOf` imap (+ d) i
      assert . not $ imap (+ d) i `isSuperintervalOf` i
    ]

  , Group "isProperSubintervalOf"
    [ (,) "antireflexivity" $ property $ do
      i <- forAll gi
      assert . not $ i `isProperSubintervalOf` i
    , (,) "transitivity" $ property $ do
      i1 <- forAll gi
      i2 <- forAll (properSuperinterval i1)
      i3 <- forAll (properSuperinterval i2)
      assert (i1 `isProperSubintervalOf` i3)
    , (,) "offset" $ property $ do
      i <- forAll gi
      d <- forAll nonZeroDelta
      assert . not $ i `isProperSubintervalOf` imap (+ d) i
      assert . not $ imap (+ d) i `isProperSubintervalOf` i
    ]

  , Group "isProperSuperintervalOf"
    [ (,) "antireflexivity" $ property $ do
      i <- forAll gi
      assert . not $ i `isProperSuperintervalOf` i
    , (,) "transitivity" $ property $ do
      i1 <- forAll gi
      i2 <- forAll (properSuperinterval i1)
      i3 <- forAll (properSuperinterval i2)
      assert (i3 `isProperSuperintervalOf` i2)
    , (,) "offset" $ property $ do
      i <- forAll gi
      d <- forAll nonZeroDelta
      assert . not $ i `isProperSuperintervalOf` imap (+ d) i
      assert . not $ imap (+ d) i `isProperSuperintervalOf` i
    ]

  , Group "union"
    [ (,) "idempotence" $ property $ do
      i <- forAll gi
      i `union` i === i
    , (,) "associativity" $ property $ do
      (i1, i2, i3) <- forAll ((,,) <$> gi <*> gi <*> gi)
      (i1 `union` i2) `union` i3 === i1 `union` (i2 `union` i3)
    , (,) "commutativity" $ property $ do
      (i1, i2) <- forAll ((,) <$> gi <*> gi)
      i1 `union` i2 === (i2 `union` i1)
    ]

  , Group "intersection"
    [ (,) "idempotence" $ property $ do
      i <- forAll gi
      i `intersection` i === i
    , (,) "associativity" $ property $ do
      (i1, i2, i3) <- forAll ((,,) <$> gi <*> gi <*> gi)
      (i1 `intersection` i2) `intersection` i3 === i1 `intersection` (i2 `intersection` i3)
    , (,) "commutativity" $ property $ do
      (i1, i2) <- forAll ((,) <$> gi <*> gi)
      i1 `intersection` i2 === (i2 `intersection` i1)
    ]

  , Group "interval"
    [ (,) "validity" $ property (forAll gi >>= assert . isValid)
    , (,) "coverage" $ verifiedTermination . withConfidence (10^(6 :: Int)) . property $ do
      i <- forAll gi
      cover 20 "point" (isPoint i)
      cover 20 "span" (inf i < sup i)
    ]

  , Group "superinterval"
    [ (,) "validity" $ property (forAll gi >>= forAll . superinterval >>= assert . isValid)
    , (,) "correctness" $ property (forAll gi >>= \ i -> forAll (superinterval i) >>= assert . isSubintervalOf i)
    , (,) "coverage" $ verifiedTermination . withConfidence (10^(6 :: Int)) . property $ do
      i <- forAll gi
      si <- forAll (superinterval i)
      cover 20 "=" (i == si)
      cover 10 "⊃" (i `isProperSubintervalOf` si)
      cover 20 "point" (isPoint si)
      cover 20 "span" (inf si < sup si)
    ]

  , Group "properSuperinterval"
    [ (,) "validity" $ property (forAll gi >>= forAll . properSuperinterval >>= assert . isValid)
    , (,) "correctness" $ property (forAll gi >>= \ i -> forAll (properSuperinterval i) >>= assert . isProperSubintervalOf i)
    , (,) "coverage" $ verifiedTermination . withConfidence (10^(6 :: Int)) . property $ do
      i <- forAll gi
      si <- forAll (properSuperinterval i)
      cover 20 "inf si == inf i" $ inf si == inf i
      cover 20 "inf si <  inf i" $ inf si <  inf i
      cover 20 "sup si == sup i" $ sup si == sup i
      cover 20 "sup si >  sup i" $ sup si >  sup i
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

superinterval :: (MonadGen m, Num a) => Interval I a -> m (Interval I a)
superinterval i = do
  l <- delta
  r <- delta
  pure $! Interval (inf i - l) (sup i + r)

properSuperinterval :: (MonadGen m, Num a) => Interval I a -> m (Interval I a)
properSuperinterval i = Gen.choice
  [ do
    l <- nonZeroDelta
    pure $! i & inf_ -~ l
  , do
    r <- nonZeroDelta
    pure $! i & sup_ +~ r
  , do
    l <- nonZeroDelta
    r <- nonZeroDelta
    pure $! i & inf_ -~ l & sup_ +~ r
  ]

delta :: (MonadGen m, Num a) => m a
delta = Gen.choice [ pure 0, fromIntegral <$> Gen.int (Range.linear 0 10) ]

nonZeroDelta :: (MonadGen m, Num a) => m a
nonZeroDelta = (+ 1) <$> delta
