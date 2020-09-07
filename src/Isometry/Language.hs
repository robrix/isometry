module Isometry.Language
( Coords(..)
, Time(..)
, Expr(..)
) where

import Linear.V3
import Unit.Length

class Coords expr where
  coords :: expr (V3 (Metres Double))

class Time expr where
  time :: expr Double

class Expr expr where
  lam  :: (expr a -> expr b) -> expr (a -> b)
  ($$) :: expr (a -> b) -> (expr a -> expr b)
  infixl 9 $$
