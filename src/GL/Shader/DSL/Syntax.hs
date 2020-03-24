{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuantifiedConstraints #-}
module GL.Shader.DSL.Syntax
( Expr(..)
, false
, true
) where

import Data.Coerce
import Data.Functor.I
import Geometry.Transform
import Linear.Matrix
import Linear.V2
import Linear.V3
import Linear.V4
import Unit.Algebra (Div, Mul)
import Unit (Unit)
import Unit.Length (Length)

class ( forall a b . Coercible a b => Coercible (expr a) (expr b)
      , forall a . Num a => Num (expr a)
      , forall a . Fractional a => Fractional (expr a)
      , forall a . Floating a => Floating (expr a)
      )
   => Expr expr where
  -- matrix

  m2 :: expr (V2 a) -> expr (V2 a) -> expr (M22 a)
  m3 :: expr (V3 a) -> expr (V3 a) -> expr (V3 a) -> expr (M33 a)
  m4 :: expr (V4 a) -> expr (V4 a) -> expr (V4 a) -> expr (V4 a) -> expr (M44 a)

  (>>>) :: expr (Transform m a u v) -> expr (Transform m a v w) -> expr (Transform m a u w)
  (>*) :: expr (Transform m a u v) -> expr (m (v a)) -> expr (m (u a))

  infixr 1 >>>
  infixl 7 >*

  (!*!) :: expr (u (v a)) -> expr (v (w a)) -> expr (u (w a))
  (!*) :: expr (u (v a)) -> expr (v a) -> expr (u a)

  infixl 7 !*!, !*

  -- vector

  v2 :: expr a -> expr a -> expr (V2 a)
  v3 :: expr a -> expr a -> expr a -> expr (V3 a)
  v4 :: expr a -> expr a -> expr a -> expr a -> expr (V4 a)

  ext3 :: expr (V2 a) -> expr a -> expr (V3 a)
  ext4 :: expr (V3 a) -> expr a -> expr (V4 a)

  norm :: expr (v a) -> expr a

  (^*) :: expr (v a) -> expr a -> expr (v a)
  (*^) :: expr a -> expr (v a) -> expr (v a)
  (^/) :: expr (v a) -> expr a -> expr (v a)

  infixl 7 ^*, *^, ^/

  -- scalar

  atan2 :: Unit Length length => expr (length Float) -> expr (length Float) -> expr (I Float)

  float :: expr (scalar a) -> expr (scalar Float)

  min, max :: expr a -> expr a -> expr a

  (<), (>), (<=), (>=), (==) :: expr a -> expr a -> expr Bool

  infix 4 <, >, <=, >=, ==

  -- units

  (.*.) :: expr (u a) -> expr (v a) -> expr (Mul u v a)
  (^*.) :: expr (f (u a)) -> expr (v a) -> expr (f (Mul u v a))
  (.*^) :: expr (u a) -> expr (f (v a)) -> expr (f (Mul u v a))
  (./.) :: expr (u a) -> expr (v a) -> expr (Div u v a)
  (^/.) :: expr (f (u a)) -> expr (v a) -> expr (f (Div u v a))
  (./^) :: expr (u a) -> expr (f (v a)) -> expr (f (Div u v a))

  infixl 7 .*., ^*., .*^, ./., ^/., ./^

  -- general syntax

  let' :: expr a -> (expr a -> expr b) -> expr b

  case' :: expr a -> [(a, expr b)] -> expr b

  -- booleans
  fromBool :: Bool -> expr Bool
  iff :: expr Bool -> expr a -> expr a -> expr a

false, true :: Expr expr => expr Bool
false = fromBool False
true  = fromBool True
