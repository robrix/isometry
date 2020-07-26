{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
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
