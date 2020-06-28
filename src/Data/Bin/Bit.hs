module Data.Bin.Bit
( Bit(..)
, toBit
, fromBit
, toBasis
) where

import Data.Bits
import Data.Bool (bool)
import Linear.Vector (E(..))
import Linear.V2

data Bit
  = B0
  | B1
  deriving (Enum, Eq, Ord, Show)

instance Bits Bit where
  -- fixme: should we avoid matching on the rhs when possible for laziness?
  B1 .&. B1 = B1
  _  .&. _  = B0

  B0 .|. B0 = B0
  _  .|. _  = B1

  a `xor` b = if a /= b then B1 else B0

  complement B0 = B1
  complement B1 = B0

  shift x 0 = x
  shift _ _ = B0

  rotate x _ = x

  bit 0 = B1
  bit _ = B0

  testBit x 0 = fromBit x
  testBit _ _ = False

  bitSizeMaybe _ = Just 1

  bitSize _ = 1

  isSigned _ = False

  popCount B0 = 0
  popCount B1 = 1

instance FiniteBits Bit where
  finiteBitSize _ = 1
  countTrailingZeros = countTrailingZeros . fromBit
  countLeadingZeros = countLeadingZeros . fromBit

toBit :: Bool -> Bit
toBit = bool B0 B1

fromBit :: Bit -> Bool
fromBit = (== B1)

toBasis :: Bit -> E V2
toBasis = bool ex ey . fromBit
