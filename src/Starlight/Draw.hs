{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Starlight.Draw
( runFrame
, frame
) where

import Control.Carrier.Empty.Church
import Control.Carrier.Reader
import Control.Carrier.State.Church
import Control.Effect.Lift
import Control.Effect.Profile
import Control.Monad.IO.Class.Lift
import Data.Time.Clock
import GL.Effect.Check
import GL.Framebuffer
import Graphics.GL.Core41
import Starlight.Input
import Starlight.Time
import Starlight.UI
import Starlight.View
import UI.Colour
import UI.Label
import UI.Typeface
import UI.Window as Window

runFrame
  :: ( Has Check sig m
     , Has (Lift IO) sig m
     )
  => StateC UTCTime (EmptyC m) a
  -> m ()
runFrame = evalEmpty . (\ m -> now >>= \ start -> evalState start m)

frame
  :: ( Has Check sig m
     , Has Empty sig m
     , Has (Lift IO) sig m
     , Has Profile sig m
     , Has (Reader UI) sig m
     , Has (Reader Window.Window) sig m
     , Has (State Input) sig m
     )
  => m ()
frame = do
  measure "input" Starlight.Input.input
  withView . measure "draw" . runLiftIO $ do
    UI{ target, face } <- ask
    let font = Font face 18
    bind @Framebuffer Nothing

    v@View{} <- ask

    clipTo v

    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

    measure "setLabel" $ setLabel target font "hello"
    measure "drawLabel" $ drawLabel target 10 white Nothing
