{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module UI.Colour
( Colour(..)
, components
, black
, white
, transparent
, red
, green
, blue
, cyan
, magenta
, yellow
, uniformRGB
, _r
, _g
, _b
, _a
, opaque
, setClearColour
, Packed(..)
, bytes
, packed
, HasColour(..)
) where

import           Control.Effect.Random
import           Control.Lens
import           Control.Monad.IO.Class.Lift
import           Data.Bits
import           Data.Generics.Product.Fields
import           Data.Word
import           Foreign.Storable (Storable)
import           GL.Type (Type)
import           GL.Uniform (Uniform)
import           Graphics.GL.Core41
import           Linear.V4
import qualified System.Random as R (Random)

newtype Colour a = Colour { getColour :: V4 a }
  deriving (Eq, Floating, Foldable, Fractional, Functor, Num, Ord, Show, Storable, Traversable, Type, Uniform)

instance R1 Colour where
  _x = _xyzw._x

instance R2 Colour where
  _y = _xyzw._y
  _xy = _xyzw._xy

instance R3 Colour where
  _z = _xyzw._z
  _xyz = _xyzw._xyz

instance R4 Colour where
  _w = _xyzw._w
  _xyzw = components._xyzw

components :: Iso (Colour a) (Colour b) (V4 a) (V4 b)
components = iso getColour Colour


black, white :: Num a => Colour a

black = Colour $ V4 0 0 0 1
white = Colour $ V4 1 1 1 1

transparent :: Num a => Colour a
transparent = Colour $ V4 0 0 0 0


red, green, blue :: Num a => Colour a

red = Colour $ V4 1 0 0 1
green = Colour $ V4 0 1 0 1
blue = Colour $ V4 0 0 1 1


cyan, magenta, yellow :: Num a => Colour a

cyan = Colour $ V4 0 1 1 1
magenta = Colour $ V4 1 0 1 1
yellow = Colour $ V4 1 1 0 1


uniformRGB :: (RealFrac a, Has Random sig m) => m (Colour a)
uniformRGB = review packed . (.|. 0xff) <$> uniform


_r :: R1 t => Lens' (t a) a
_r = _x

_g :: R2 t => Lens' (t a) a
_g = _y

_b :: R3 t => Lens' (t a) a
_b = _z

_a :: R4 t => Lens' (t a) a
_a = _w


opaque :: Num a => Colour a -> Colour a
opaque = set _a 1


setClearColour :: (Real a, Has (Lift IO) sig m) => Colour a -> m ()
setClearColour (fmap realToFrac -> Colour (V4 r g b a)) = runLiftIO $ glClearColor r g b a


newtype Packed = Packed { getPacked :: Word32 }
  deriving (Bits, Enum, Eq, Integral, Num, Ord, R.Random, Real, Show, Storable, Type, Uniform)

bytes :: (RealFrac a, Fractional b, Integral i) => Iso (Colour a) (Colour b) (V4 i) (V4 i)
bytes = components.iso to from
  where
  to = fmap (round . (* 255))
  from = fmap ((/ 255) . fromIntegral)

packed :: (RealFrac a, Fractional b) => Iso (Colour a) (Colour b) Packed Packed
packed = components.iso pack unpack
  where
  pack (fmap (round . (* 255)) -> V4 r g b a) = shiftL r 24 .|. shiftL g 16 .|. shiftL b 8 .|. a :: Packed
  unpack i = (/ 255) . fromIntegral <$> V4 (0xff .&. shiftR i 24) (0xff .&. shiftR i 16) (0xff .&. shiftR i 8) (0xff .&. i)


class HasColour t where
  colour_ :: Lens' t (Colour Float)
  default colour_ :: HasField "colour" t t (Colour Float) (Colour Float) => Lens' t (Colour Float)
  colour_ = field @"colour"

instance HasColour (Colour Float) where
  colour_ = id

instance HasColour (V4 Float) where
  colour_ = coerced
