{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
module Isometry.Draw.Axis
( draw
, runDrawable
, Drawable
) where

import           Control.Carrier.Reader
import           Control.Effect.Finally
import           Control.Effect.Lens ((?=))
import           Control.Effect.Lift
import           Control.Effect.Trace
import           Control.Lens (Lens')
import           Data.Functor.I
import           Data.Functor.Interval hiding (range)
import           Data.Generics.Product.Fields
import           Foreign.Storable
import           Geometry.Transform
import           GHC.Generics
import           GL.Array
import           GL.Effect.Check
import           GL.Shader.DSL as D hiding (get, (.*.), (./.), (^.), _x, _y, _z)
import           Isometry.View as View
import           Linear.V3
import           Linear.Vector
import           UI.Colour as UI
import qualified UI.Drawable as UI
import           Unit.Length

draw
  :: ( Has Check sig m
     , Has (Lift IO) sig m
     , Has (Reader Drawable) sig m
     , Has (Reader View) sig m
     )
  => m ()
draw = UI.using getDrawable $ do
  v <- ask
  matrix_ ?= tmap realToFrac (transformToWorld v)
  drawArrays Lines range


runDrawable
  :: ( Has Check sig m
     , Has Finally sig m
     , Has (Lift IO) sig m
     , Has Trace sig m
     )
  => ReaderC Drawable m a
  -> m a
runDrawable = UI.loadingDrawable Drawable shader vertices


newtype Drawable = Drawable { getDrawable :: UI.Drawable U V Frag }


vertices :: [V I]
vertices =
  [ V (I v) (I colour) | (axis, colour) <- axes, v <- [0, axis] ]
  where
  axes =
    [ ( unit _x, UI.red)
    , ( unit _y, UI.green)
    , ( unit _z, UI.blue)
    , (-unit _x, UI.cyan)
    , (-unit _y, UI.magenta)
    , (-unit _z, UI.yellow)
    ]

range :: Interval I Int
range = 0...length vertices


shader :: D.Shader shader => shader U V Frag
shader
  =   vertex (\ U{ matrix } V{ pos, colour } IF{ colour2 } -> main $ do
    gl_Position .= matrix D.>* ext4 (pos * 10) 1
    colour2 .= colour)

  >>> fragment (\ _ IF{ colour2 } Frag{ fragColour } -> main $
    fragColour .= colour2)


newtype U v = U
  { matrix :: v (Transform V4 Float Metres ClipUnits)
  }
  deriving (Generic)

instance D.Vars U

matrix_ :: Lens' (U v) (v (Transform V4 Float Metres ClipUnits))
matrix_ = field @"matrix"


newtype IF v = IF
  { colour2 :: v (Colour Float)
  }
  deriving (Generic)

instance D.Vars IF


data V v = V
  { pos    :: v (V3 (Metres Float))
  , colour :: v (Colour Float)
  }
  deriving (Generic)

instance D.Vars V

deriving via Fields V instance Storable (V I)
