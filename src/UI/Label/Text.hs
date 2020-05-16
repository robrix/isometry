{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
module UI.Label.Text
( shader
, U(..)
, sampler_
, colour_
, V(..)
, Frag(..)
) where

import Control.Lens (Lens')
import Data.Coerce
import Data.Functor.I
import Data.Generics.Product.Fields
import Foreign.Storable (Storable)
import GHC.Generics (Generic)
import GL.Shader.DSL

shader :: Shader shader => shader U V Frag
shader
  =   vertex (\ _ V{ pos } IF{ uv } -> main $ do
    uv .= (pos * xy 1 (-1)) * 0.5 + 0.5
    gl_Position .= coerce (ext4 (ext3 (pos * xy 1 (-1)) 0) 1))

  >>> fragment (\ U{ sampler, colour } IF{ uv } Frag{ fragColour } -> main $ do
    -- Get samples for -2/3 and -1/3
    valueL <- let' "valueL" $ texture sampler (xy (uv^._x + dFdx (uv^._x)) (uv^._y))^._yz * 255
    lowerL <- let' "lowerL" $ mod' valueL 16
    upperL <- let' "upperL" $ (valueL - lowerL) / 16
    alphaL <- let' "alphaL" $ min' (abs (upperL - lowerL)) 2

    -- Get samples for 0, +1/3, and +2/3
    valueR <- let' "valueR" $ texture sampler uv ^. _xyz * 255
    lowerR <- let' "lowerR" $ mod' valueR 16
    upperR <- let' "upperR" $ (valueR - lowerR) / 16
    alphaR <- let' "alphaR" $ min' (abs (upperR - lowerR)) 2

    -- Average the energy over the pixels on either side
    rgba <- let' "rgba" $ xyzw
      ((alphaR ^. _x + alphaR ^. _y + alphaR ^. _z) / 6)
      ((alphaL ^. _y + alphaR ^. _x + alphaR ^. _y) / 6)
      ((alphaL ^. _x + alphaL ^. _y + alphaR ^. _x) / 6)
      0

    iff (colour ^. _x `eq` 0)
      (fragColour .= 1 - rgba)
      (fragColour .= colour * rgba))


data U v = U
  { sampler :: v TextureUnit
  , colour  :: v (Colour Float)
  }
  deriving (Generic)

instance Vars U

sampler_ :: Lens' (U v) (v TextureUnit)
sampler_ = field @"sampler"

colour_  :: Lens' (U v) (v (Colour Float))
colour_ = field @"colour"


newtype V v = V { pos :: v (V2 Float) }
  deriving (Generic)

instance Vars V

deriving via Fields V instance Storable (V I)

newtype IF v = IF { uv :: v (V2 Float) }
  deriving (Generic)

instance Vars IF
