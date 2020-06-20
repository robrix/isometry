module Data.Bit
( Bit(..)
, toBit
, fromBit
) where

import Data.Bits
import Data.Bool (bool)

data Bit
  = I0
  | I1
  deriving (Enum, Eq, Ord, Show)

toBit :: Bool -> Bit
toBit = bool I0 I1

fromBit :: Bit -> Bool
fromBit = (== I1)

instance Bits Bit where
  -- fixme: should we avoid matching on the rhs when possible for laziness?
  I1 .&. I1 = I1
  _  .&. _  = I0

  I0 .|. I0 = I0
  _  .|. _  = I1

  a `xor` b = if a /= b then I1 else I0

  complement I0 = I1
  complement I1 = I0

  shift x 0 = x
  shift _ _ = I0

  rotate x _ = x

  bit 0 = I1
  bit _ = I0

  testBit x 0 = fromBit x
  testBit _ _ = False

  bitSizeMaybe _ = Just 1

  bitSize _ = 1

  isSigned _ = False

  popCount I0 = 0
  popCount I1 = 1

instance FiniteBits Bit where
  finiteBitSize _ = 1
  countTrailingZeros = countTrailingZeros . fromBit
  countLeadingZeros = countLeadingZeros . fromBit
