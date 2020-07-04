{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Bin.Shape
( N(..)
, S1
, S2
, S4
, S8
, S16
, S32
, S64
, S128
, S256
, S512
, S1024
, S2048
, S4096
, S8192
, Size
, size
, Place
, place
) where

import Data.Nat
import Data.Proxy
import GHC.TypeLits

type S1 = 'Z
type S2 = 'S S1
type S4 = 'S S2
type S8 = 'S S4
type S16 = 'S S8
type S32 = 'S S16
type S64 = 'S S32
type S128 = 'S S64
type S256 = 'S S128
type S512 = 'S S256
type S1024 = 'S S512
type S2048 = 'S S1024
type S4096 = 'S S2048
type S8192 = 'S S4096


type family Size (b :: N) :: Nat where
  Size 'Z     = 1
  Size ('S s) = 2 * Size s

size :: forall s t a . KnownNat (Size s) => t s a -> Int
size _ = fromIntegral (natVal (Proxy @(Size s)))


type family Place (b :: N) :: Nat where
  Place 'Z     = 0
  Place ('S s) = 1 + Place s

-- | Produce the place of the bit for a given shape; equivalently, the power of two that the shape represents.
--
-- @
-- size i = 2^place i
-- @
place :: forall s i . KnownNat (Place s) => i s -> Int
place _ = fromIntegral (natVal (Proxy @(Place s)))
