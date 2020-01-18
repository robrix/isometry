{-# LANGUAGE DeriveFunctor #-}
module Geometry.Triangle
( Triangle(..)
, Kind(..)
, triangleVertices
) where

import Linear.V2
import Linear.V4

data Triangle n = Triangle
  {-# UNPACK #-} !(V2 n)
  {-# UNPACK #-} !(V2 n)
  {-# UNPACK #-} !(V2 n)
  deriving (Eq, Functor, Show)

data Kind = Solid | Curve
  deriving (Eq, Show)


triangleVertices :: Num a => Triangle a -> Kind -> [V4 a]
triangleVertices (Triangle (V2 ax ay) (V2 bx by) (V2 cx cy)) Solid = [ V4 ax ay 0 2, V4 bx by 0 2, V4 cx cy 0 2 ]
triangleVertices (Triangle (V2 ax ay) (V2 bx by) (V2 cx cy)) Curve = [ V4 ax ay 0 0, V4 bx by 1 0, V4 cx cy 2 2 ]
