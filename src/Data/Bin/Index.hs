{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
module Data.Bin.Index
( Index(..)
, fromIndex
, toFraction
) where

import Data.Bin.Bit
import Data.Bin.Shape

data Index i where
  IL :: Index 'S1
  IB :: Bit -> Index i -> Index ('S2x i)

deriving instance Eq   (Index i)
deriving instance Ord  (Index i)
deriving instance Show (Index i)

fromIndex :: Index ('S2x i) -> (Bit, Index i)
fromIndex (IB b i) = (b, i)

toFraction :: Index i -> (Integer, Integer)
toFraction = go
  where
  go :: Index i -> (Integer, Integer)
  go IL       = (0, 1)
  go (IB b i) = let (n, d) = go i in case b of
    I0 -> (n, d * 2)
    I1 -> (n + d, d * 2)
