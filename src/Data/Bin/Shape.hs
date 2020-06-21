{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Bin.Shape
( Shape(..)
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
) where

import GHC.TypeLits

-- | The shape of (non-empty) perfectly balanced binary trees.
--
-- This represents shapes of size 2ⁿ.
data Shape
  = S1         -- 1
  | S2x !Shape -- 2 * n

type S2 = 'S2x 'S1
type S4 = 'S2x S2
type S8 = 'S2x S4
type S16 = 'S2x S8
type S32 = 'S2x S16
type S64 = 'S2x S32
type S128 = 'S2x S64
type S256 = 'S2x S128
type S512 = 'S2x S256
type S1024 = 'S2x S512
type S2048 = 'S2x S1024
type S4096 = 'S2x S2048
type S8192 = 'S2x S4096


type family Size (b :: Shape) :: Nat where
  Size 'S1      = 1
  Size ('S2x s) = 2 * Size s
