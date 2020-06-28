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
import Data.Word
import GHC.TypeLits
import Numeric (showIntAtBase)

type role Index representational

newtype Index (i :: Shape) = Index { getIndex :: Word32 }
  deriving (Eq, Ord)

instance KnownNat (Place i) => Bounded (Index i) where
  minBound = Index 0
  {-# INLINABLE minBound #-}
  maxBound = let i = Index (bit (place i) - 1) in i
  {-# INLINABLE maxBound #-}

instance KnownNat (Place i) => Enum (Index i) where
  toEnum i | i' <- fromIntegral i
           , i' <= getIndex (maxBound :: Index i) = Index i'
           | otherwise = error "Data.Bin.Index.Index.toEnum: bad argument"
  {-# INLINABLE toEnum #-}
  fromEnum = fromIntegral . getIndex
  {-# INLINABLE fromEnum #-}

instance KnownNat (Place i) => Bits (Index i) where
  Index a .&. Index b = Index (a .&. b)
  {-# INLINABLE (.&.) #-}
  Index a .|. Index b = Index (a .|. b)
  {-# INLINABLE (.|.) #-}
  Index a `xor` Index b = Index (a `xor` b)
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
  shift i p = Index (shift (getIndex i) p) .&. maxBound
  {-# INLINABLE shift #-}
  rotate i p =
    let bits = finiteBitSize i
        p' = p `mod` bits
    in Index (shift (getIndex i) p' .|. shift (getIndex i) (-(bits - p'))) .&. maxBound
  {-# INLINABLE rotate #-}

instance KnownNat (Place i) => FiniteBits (Index i) where
  finiteBitSize = place
  {-# INLINABLE finiteBitSize #-}

instance Show (Index i) where
  showsPrec p (Index i) = showsUnaryWith (const (fmap (('0':) . ('b':)) . showIntAtBase 2 intToDigit)) "Index" p i
  {-# INLINABLE showsPrec #-}

il :: Index 'S1
il = Index 0
{-# INLINABLE il #-}

ib :: Bit -> Index s -> Index ('S2x s)
ib B0 (Index i) = Index (shift i 1)
ib B1 (Index i) = Index (shift i 1 .|. 1)
{-# INLINABLE ib #-}

i0, i1 :: Index s -> Index ('S2x s)
i0 = ib B0
{-# INLINABLE i0 #-}
i1 = ib B1
{-# INLINABLE i1 #-}

decompose :: Index ('S2x i) -> (Bit, Index i)
decompose (Index i) = (toBit (testBit i 0), Index (shift i (-1)))
{-# INLINABLE decompose #-}

toInt :: Index i -> Int
toInt = fromIntegral . getIndex
{-# INLINABLE toInt #-}
