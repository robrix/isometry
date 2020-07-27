{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
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
import GHC.TypeLits
import Numeric (showIntAtBase)

type role Index representational

newtype Index (i :: N) = Index { getIndex :: Word32 }
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

instance KnownN i => Show (Index i) where
  showsPrec p = showsUnaryWith (const (fmap (('0':) . ('b':)) . showIntAtBase 2 intToDigit)) "Index" p . toInt
  {-# INLINABLE showsPrec #-}

il :: Index 'Z
il = Index 0
{-# INLINABLE il #-}

ib :: Bit -> Index s -> Index ('S s)
ib B0 (Index i) = Index (shift i 1)
ib B1 (Index i) = Index (shift i 1 .|. 1)
{-# INLINABLE ib #-}

i0, i1 :: Index s -> Index ('S s)
i0 = ib B0
{-# INLINABLE i0 #-}
i1 = ib B1
{-# INLINABLE i1 #-}

decompose :: Index ('S i) -> (Bit, Index i)
decompose (Index i) = (toBit (testBit i 0), Index (shift i (-1)))
{-# INLINABLE decompose #-}

toInt :: KnownN i => Index i -> Int
toInt (i :: Index i) = go (reifyN @i) (getIndex i) 0
  where
  go Z     _ j = j
  go (S n) i j = go n (shift i (-1)) (shift j 1 .|. if testBit i 0 then 1 else 0)
{-# INLINABLE toInt #-}
