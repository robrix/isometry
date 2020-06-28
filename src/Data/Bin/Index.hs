{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
module Data.Bin.Index
( Index(..)
, decompose
, toInt
) where

import Data.Bin.Bit
import Data.Bin.Shape
import Data.Proxy
import GHC.TypeLits

data Index i where
  IL :: Index 'S1
  IB :: !Bit -> !(Index i) -> Index ('S2x i)

deriving instance Eq   (Index i)
deriving instance Ord  (Index i)
deriving instance Show (Index i)

decompose :: Index ('S2x i) -> (Bit, Index i)
decompose (IB b i) = (b, i)

toInt :: forall i . KnownNat (Size i) => Index i -> Int
toInt = go (fromIntegral (natVal (Proxy @(Size i))))
  where
  go :: Int -> Index i' -> Int
  go _ IL       = 0
  go d (IB b i) = let n = go d' i in case b of
    I0 -> n
    I1 -> n + d'
    where
    d' = d `div` 2
