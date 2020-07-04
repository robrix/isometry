module Data.Nat
( N(..)
) where

data N
  = Z
  | S N
  deriving (Eq, Ord, Show)
