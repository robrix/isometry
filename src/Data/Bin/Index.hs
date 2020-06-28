{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
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
import GHC.TypeLits

type role Index representational

newtype Index (i :: Shape) = Index { getIndex :: Word32 }
  deriving (Eq, Ord, Show)

instance KnownNat (Place i) => Bounded (Index i) where
  minBound = Index 0
  maxBound = let i = Index (bit (place i) - 1) in i

instance KnownNat (Place i) => Enum (Index i) where
  toEnum i | i' <- fromIntegral i
           , i' <= getIndex (maxBound :: Index i) = Index i'
           | otherwise = error "Data.Bin.Index.Index.toEnum: bad argument"
  fromEnum = fromIntegral . getIndex

il :: Index 'S1
il = Index 0

ib :: Bit -> Index s -> Index ('S2x s)
ib B0 (Index i) = Index (shift i 1)
ib B1 (Index i) = Index (shift i 1 .|. 1)

decompose :: Index ('S2x i) -> (Bit, Index i)
decompose (Index i) = (toBit (testBit i 0), Index (shift i (-1)))

toInt :: Index i -> Int
toInt = fromIntegral . getIndex
