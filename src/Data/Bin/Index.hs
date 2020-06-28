{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RoleAnnotations #-}
module Data.Bin.Index
( Index
, il
, ib
, decompose
, toInt
) where

import Data.Bin.Bit
import Data.Bin.Shape
import Data.Bits
import Data.Word

type role Index representational

newtype Index (i :: Shape) = Index { getIndex :: Word32 }
  deriving (Eq, Ord, Show)

instance Bounded (Index 'S1) where
  minBound = il
  maxBound = il

instance Enum (Index 'S1) where
  toEnum 0 = il
  toEnum _ = error "Data.Bin.Index.Index 'S1.toEnum: bad argument"
  fromEnum _ = 0

il :: Index 'S1
il = Index 0

ib :: Bit -> Index s -> Index ('S2x s)
ib B0 (Index i) = Index (shift i 1)
ib B1 (Index i) = Index (shift i 1 .|. 1)

decompose :: Index ('S2x i) -> (Bit, Index i)
decompose (Index i) = (toBit (testBit i 0), Index (shift i (-1)))

toInt :: Index i -> Int
toInt = fromIntegral . getIndex
