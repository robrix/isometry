{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
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
import           Control.Effect.Labelled
import           Control.Effect.Lens (use, (%=))
import           Control.Effect.Lift
import           Control.Effect.Profile
import           Control.Effect.Trace
import           Control.Lens (Lens', (^.))
import           Control.Monad (when)
import           Control.Monad.IO.Class.Lift
import           Data.Functor.I
import           Data.Functor.Interval hiding (range)
import           Data.Generics.Product.Fields
import           Data.Time.Clock
import           GHC.Generics (Generic)
import           GL.Effect.Check
import           GL.Framebuffer
import           Graphics.GL.Core41
import qualified Isometry.Draw.Axis as Axis
import qualified Isometry.Draw.Voxel as Voxel
import           Isometry.Input as Input
import           Isometry.Octree as Octree (Shape(..), tetra)
import           Isometry.Time
import           Isometry.UI
import           Isometry.View as View
import           Isometry.Voxel as Voxel
import           Isometry.World
import qualified SDL
import qualified UI.Colour as UI
import           UI.Label
import           UI.Typeface
import           UI.Window as Window
import           Unit.Angle
import           Unit.Time

runFrame
  :: ( Has Check sig m
     , Has Finally sig m
     , Has (Lift IO) sig m
     , Has Trace sig m
     )
  => ReaderC Voxel.Drawable
    (ReaderC (Interval I Int)
    (ReaderC Axis.Drawable
    (Labelled World (ReaderC (Octree ('S2x ('S2x ('S2x 'S1))) Voxel))
    (StateC UTCTime
    (StateC Player
    (EmptyC
    m)))))) a
  -> m ()
runFrame
  = evalEmpty
  . evalState Player{ angle = -pi/4 }
  . (\ m -> now >>= \ start -> evalState start m)
  . runReader (Octree (tetra (const (Voxel UI.red))))
  . runLabelled
  . Axis.runDrawable
  . Voxel.runDrawable

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
     , Has (Reader Voxel.Drawable) sig m
     , Has (Reader (Interval I Int)) sig m
     , Has (Reader UI) sig m
     , Has (Reader Window.Window) sig m
     , Has (State Input) sig m
     , Has (State Player) sig m
     , Has (State UTCTime) sig m
     )
  => m ()
frame = timed $ do
  measure "input" Input.input

  dt <- ask @(Seconds _)
  input <- get @Input

  when (input^.pressed_ SDL.KeycodeQ) $ angle_ %= \ angle -> wrap radians (angle + (-turnRate .*. dt))
  when (input^.pressed_ SDL.KeycodeE) $ angle_ %= \ angle -> wrap radians (angle +   turnRate .*. dt)

  angle <- use angle_

  withView angle . measure "draw" . runLiftIO $ do
    UI{ target, face } <- ask
    let font = Font face 18
    bind @Framebuffer Nothing

    clipTo =<< ask

    glClearColor 0 0 0 0
    glClear GL_COLOR_BUFFER_BIT

    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

    Axis.draw
    Voxel.draw

    measure "setLabel" $ setLabel target font "hello"
    measure "drawLabel" $ drawLabel target 10 UI.white Nothing
  where
  turnRate :: (I :/: Seconds) Double
  turnRate = I pi ./. Seconds 1
