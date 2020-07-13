{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE DataKinds #-}
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
( runDrawables
, draw
) where

import           Control.Carrier.Empty.Church
import           Control.Carrier.Reader
import           Control.Effect.Finally
import           Control.Effect.Labelled
import           Control.Effect.Lift
import           Control.Effect.Profile
import           Control.Effect.Trace
import           Data.Bits ((.|.))
import           GHC.Stack
import           GL.Effect.Check
import           GL.Framebuffer
import           Graphics.GL.Core41
import qualified Isometry.Draw.Axis as Axis
import qualified Isometry.Draw.Voxel as Voxel
import           Isometry.UI
import           Isometry.View as View
import           Isometry.Voxel as Voxel
import           Isometry.World
import qualified UI.Colour as UI
import           UI.Context
import           UI.Label
import           UI.Typeface
import           UI.Window as Window

runDrawables
  :: ( Has Check sig m
     , Has Finally sig m
     , Has (Lift IO) sig m
     , Has Profile sig m
     , HasLabelled World (Reader (World s Voxel)) sig m
     , Has Trace sig m
     )
  => ReaderC Voxel.Drawable
    (ReaderC Axis.Drawable
    m) a
  -> m a
runDrawables
  = Axis.runDrawable
  . Voxel.runDrawable

draw
  :: ( Has Check sig m
     , Has (Lift IO) sig m
     , Has Profile sig m
     , Has (Reader Axis.Drawable) sig m
     , Has (Reader Voxel.Drawable) sig m
     , Has (Reader UI) sig m
     , Has (Reader View) sig m
     , Has (Reader Window.Window) sig m
     , HasLabelled World (Reader (World s Voxel)) sig m
     , HasCallStack
     )
     => m ()
draw = do
  UI{ target, face } <- ask
  let font = Font face 18
  bind @Framebuffer Nothing

  clipToContext

  sendIO $ glDepthMask GL_TRUE

  UI.setClearColour UI.black
  sendIO . glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT

  sendIO $ glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

  measure "Axis.draw" Axis.draw
  measure "Voxel.draw" Voxel.draw

  sendIO $ glDepthMask GL_FALSE

  measure "setLabel" $ setLabel target font "hello"
  measure "drawLabel" $ drawLabel target 10 UI.white Nothing

  measure "glFinish" (sendIO glFinish)
