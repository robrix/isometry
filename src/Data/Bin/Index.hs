{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Bin.Index
( Index
, il
, ib
, i0
, i1
, decompose
, toInt
) where

import Data.Bin.Bit
import Data.Bin.Shape
import Data.Bits
import Data.Char (intToDigit)
import Data.Coerce
import Data.Functor.Classes (showsUnaryWith)
import Data.Nat
import Data.Word
import Numeric (showIntAtBase)

type role Index representational

newtype Index (i :: N) = Index { getIndex :: Word32 }
  deriving (Eq, Ord)

instance Place i => Bounded (Index i) where
  minBound = Index 0
  {-# INLINABLE minBound #-}
  maxBound = let i = Index (bit (place i) - 1) in i
  {-# INLINABLE maxBound #-}

instance Place i => Enum (Index i) where
  toEnum i | i' <- fromIntegral i
           , i' <= getIndex (maxBound :: Index i) = Index i'
           | otherwise = error "Data.Bin.Index.Index.toEnum: bad argument"
  {-# INLINABLE toEnum #-}
  fromEnum = fromIntegral . getIndex
  {-# INLINABLE fromEnum #-}

instance Place i => Bits (Index i) where
  (.&.) = coerce ((.&.) :: Word32 -> Word32 -> Word32)
  {-# INLINABLE (.&.) #-}
  (.|.) = coerce ((.|.) :: Word32 -> Word32 -> Word32)
  {-# INLINABLE (.|.) #-}
  xor = coerce (xor :: Word32 -> Word32 -> Word32)
  {-# INLINABLE xor #-}
  zeroBits = Index 0
  {-# INLINABLE zeroBits #-}
  bitSize = finiteBitSize
  {-# INLINABLE bitSize #-}
  bitSizeMaybe = Just . finiteBitSize
  {-# INLINABLE bitSizeMaybe #-}
  isSigned _ = False
  {-# INLINABLE isSigned #-}
  popCount = coerce (popCount :: Word32 -> Int)
  {-# INLINABLE popCount #-}
  complement = xor maxBound
  {-# INLINABLE complement #-}
  testBit i p
    | p > place i = False
    | otherwise   = testBit (getIndex i) p
  {-# INLINABLE testBit #-}
  bit p = Index (bit p) .&. maxBound
  {-# INLINABLE bit #-}
  setBit i p = Index (setBit (getIndex i) p) .&. maxBound
  {-# INLINABLE setBit #-}
  clearBit = coerce (clearBit :: Word32 -> Int -> Word32)
  {-# INLINABLE clearBit #-}
  complementBit i p = Index (complementBit (getIndex i) p) .&. maxBound
  {-# INLINABLE complementBit #-}
  shift i p = Index (shift (getIndex i) p) .&. maxBound
  {-# INLINABLE shift #-}
  rotate i p =
    let bits = finiteBitSize i
        p' = p `mod` bits
    in Index (shift (getIndex i) p' .|. shift (getIndex i) (-(bits - p'))) .&. maxBound
  {-# INLINABLE rotate #-}

instance Place i => FiniteBits (Index i) where
  finiteBitSize = place
  {-# INLINABLE finiteBitSize #-}

instance KnownN i => Show (Index i) where
  showsPrec p = showsUnaryWith (const (fmap (('0':) . ('b':)) . showIntAtBase 2 intToDigit)) "Index" p . toInt
  {-# INLINABLE showsPrec #-}

il :: Index 'Z
il = Index 0
{-# INLINABLE il #-}

ib :: Place s => Bit -> Index s -> Index ('S s)
ib B0 i = Index (getIndex i)
ib B1 i = Index (setBit (getIndex i) (place i))
{-# INLINABLE ib #-}

i0, i1 :: Place s => Index s -> Index ('S s)
i0 = ib B0
{-# INLINABLE i0 #-}
i1 = ib B1
{-# INLINABLE i1 #-}

decompose :: Place i => Index ('S i) -> (Bit, Index i)
decompose i = (toBit (testBit (getIndex i) p), i')
  where
  p = place i'
  i' = Index (clearBit (getIndex i) p)
{-# INLINABLE decompose #-}

toInt :: Index i -> Int
toInt = fromIntegral . getIndex
{-# INLINABLE toInt #-}
