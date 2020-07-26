{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Data.Nat
( N(..)
, KnownN(..)
) where

data N
  = Z
  | S N
  deriving (Eq, Ord, Show)

class KnownN (n :: N) where
  reifyN :: N

instance KnownN 'Z where
  reifyN = Z

instance KnownN n => KnownN ('S n) where
  reifyN = S (reifyN @n)
