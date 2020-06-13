{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Isometry.Draw
( runFrame
, frame
) where

import           Control.Carrier.Empty.Church
import           Control.Carrier.Reader
import           Control.Carrier.State.Church
import           Control.Effect.Finally
import           Control.Effect.Lens (use, (%=), (?=))
import           Control.Effect.Lift
import           Control.Effect.Profile
import           Control.Effect.Trace
import           Control.Lens (Lens', (^.))
import           Control.Monad (when)
import           Control.Monad.IO.Class.Lift
import           Data.Coerce
import           Data.Functor.I
import           Data.Functor.Interval hiding (range)
import           Data.Generics.Product.Fields
import           Data.Time.Clock
import           Foreign.Storable (Storable)
import           GHC.Generics (Generic)
import           GL.Array
import           GL.Effect.Check
import           GL.Framebuffer
import           GL.Shader.DSL as D hiding (get, (.*.), (./.), (^.), _x, _y, _z)
import           Graphics.GL.Core41
import qualified Isometry.Draw.Axis as Axis
import           Isometry.Input as Input
import           Isometry.Time
import           Isometry.UI
import           Isometry.View as View
import qualified SDL
import qualified UI.Colour as UI
import qualified UI.Drawable as UI
import           UI.Label
import           UI.Typeface
import           UI.Window as Window
import           Unit.Angle
import           Unit.Length
import           Unit.Time

runFrame
  :: ( Has Check sig m
     , Has Finally sig m
     , Has (Lift IO) sig m
     , Has Trace sig m
     )
  => ReaderC Drawable
    (ReaderC Axis.Drawable
    (ReaderC (Seconds Double)
    (StateC UTCTime
    (StateC Player
    (EmptyC
    m))))) a
  -> m ()
runFrame = evalEmpty . evalState Player{ angle = pi/4 } . (\ m -> now >>= \ start -> evalState start m) . timed . Axis.runDrawable . runDrawable

newtype Player = Player
  { angle :: I Double
  }
  deriving (Generic)

angle_ :: Lens' Player (I Double)
angle_ = field @"angle"

frame
  :: ( Has Check sig m
     , Has Empty sig m
     , Has (Lift IO) sig m
     , Has Profile sig m
     , Has (Reader Axis.Drawable) sig m
     , Has (Reader Drawable) sig m
     , Has (Reader (Seconds Double)) sig m
     , Has (Reader UI) sig m
     , Has (Reader Window.Window) sig m
     , Has (State Input) sig m
     , Has (State Player) sig m
     )
  => m ()
frame = do
  measure "input" Input.input

  dt <- ask @(Seconds _)
  input <- get @Input

  when (input^.pressed_ SDL.KeycodeQ) $ angle_ %= \ angle -> wrap radians (angle +   turnRate .*. dt)
  when (input^.pressed_ SDL.KeycodeE) $ angle_ %= \ angle -> wrap radians (angle + (-turnRate .*. dt))

  angle <- use angle_

  withView angle . measure "draw" . runLiftIO $ do
    UI{ target, face } <- ask
    let font = Font face 18
    bind @Framebuffer Nothing

    v@View{} <- ask

    clipTo v

    glClearColor 0 0 0 0
    glClear GL_COLOR_BUFFER_BIT

    Axis.draw

    UI.using getDrawable $ do
      matrix_ ?= tmap realToFrac (transformToWorld v)

      drawArrays Triangles range

    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

    measure "setLabel" $ setLabel target font "hello"
    measure "drawLabel" $ drawLabel target 10 UI.white Nothing
  where
  turnRate :: (I :/: Seconds) Double
  turnRate = I (2000 * pi) ./. Seconds 1
  -- fixme: 2000 radians per second? why??


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
vertices = coerce @[V3 (Metres Float)]
  [ -- far
    V3 (-1) (-1) (-1)
  , V3   1  (-1) (-1)
  , V3   1    1  (-1)

  , V3   1    1  (-1)
  , V3 (-1)   1  (-1)
  , V3 (-1) (-1) (-1)

    -- near
  , V3 (-1) (-1)   1
  , V3   1  (-1)   1
  , V3   1    1    1

  , V3   1    1    1
  , V3 (-1)   1    1
  , V3 (-1) (-1)   1

    -- left
  , V3 (-1) (-1) (-1)
  , V3 (-1)   1  (-1)
  , V3 (-1)   1    1

  , V3 (-1)   1    1
  , V3 (-1) (-1)   1
  , V3 (-1) (-1) (-1)

    -- right
  , V3   1  (-1) (-1)
  , V3   1    1  (-1)
  , V3   1    1    1

  , V3   1    1    1
  , V3   1  (-1)   1
  , V3   1  (-1) (-1)

    -- bottom
  , V3 (-1) (-1) (-1)
  , V3   1  (-1) (-1)
  , V3   1  (-1)   1

  , V3   1  (-1)   1
  , V3 (-1) (-1)   1
  , V3 (-1) (-1) (-1)

    -- top
  , V3 (-1)   1  (-1)
  , V3   1    1  (-1)
  , V3   1    1    1

  , V3   1    1    1
  , V3 (-1)   1    1
  , V3 (-1)   1  (-1)
  ]

range :: Interval I Int
range = 0...length vertices


shader :: D.Shader shader => shader U V Frag
shader
  =   vertex (\ U{ matrix } V{ pos } None -> main $
    gl_Position .= matrix D.>* ext4 pos 1)

  >>> fragment (\ _ None Frag{ fragColour } -> main $
    fragColour .= v4 UI.white)


newtype U v = U
  { matrix :: v (Transform V4 Float Metres ClipUnits)
  }
  deriving (Generic)

instance D.Vars U

matrix_ :: Lens' (U v) (v (Transform V4 Float Metres ClipUnits))
matrix_ = field @"matrix"


newtype V v = V { pos :: v (V3 (Metres Float)) }
  deriving (Generic)

instance D.Vars V

deriving via Fields V instance Storable (V I)
