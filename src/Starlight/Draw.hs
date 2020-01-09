{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Starlight.Draw
( draw
) where

import Control.Effect.Lens (view)
import Control.Effect.Lift
import Control.Effect.Profile
import Control.Effect.Reader
import Control.Lens (forOf_, traversed, (^.))
import Control.Monad (when)
import Control.Monad.IO.Class.Lift
import Data.Foldable (for_)
import Data.Functor.Interval
import GL.Effect.Check
import GL.Framebuffer
import GL.Viewport
import Graphics.GL.Core41
import Linear.Exts
import Numeric
import Starlight.Actor
import Starlight.Body as Body
import Starlight.Character as Character
import Starlight.Draw.Body as Body
import Starlight.Draw.Radar as Radar
import Starlight.Draw.Ship as Ship
import Starlight.Draw.Starfield as Starfield
import Starlight.Draw.Weapon.Laser as Laser
import Starlight.Identifier
import Starlight.System
import Starlight.UI
import Starlight.View
import UI.Colour
import UI.Label
import UI.Typeface
import Unit.Length
import Unit.Time

draw
  :: ( Has Check sig m
     , Has (Lift IO) sig m
     , Has Profile sig m
     , Has (Reader Body.Drawable) sig m
     , Has (Reader Laser.Drawable) sig m
     , Has (Reader Radar.Drawable) sig m
     , Has (Reader Ship.Drawable) sig m
     , Has (Reader Starfield.Drawable) sig m
     , Has (Reader (Delta Seconds Float)) sig m
     , Has (Reader (System StateVectors)) sig m
     , Has (Reader UI) sig m
     , Has (Reader View) sig m
     )
  => m ()
draw = measure "draw" . runLiftIO $ do
  dt <- ask @(Delta Seconds Float)
  UI{ fps = fpsLabel, target = targetLabel, face } <- ask
  let font = Font face 18
  bind @Framebuffer Nothing

  v@View{ size, zoom } <- ask
  system@System{ scale, beams } <- ask @(System StateVectors)
  Character{ actor = Actor{ position }, target } <- view (player_ @StateVectors)

  let dsize = deviceSize v

  viewport $ Interval 0 dsize
  scissor  $ Interval 0 dsize

  glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

  drawStarfield

  for_ (system^.characters_) drawShip

  for_ beams drawLaser

  let maxDim = maximum (fromIntegral <$> dsize) * zoom
      onScreen StateVectors{ body = Body{ radius }, actor = Actor{ position = pos } } = distance pos position - scale * getMetres (getKilo radius) < maxDim * 0.5

  forOf_ (bodies_ . traversed) system $ \ sv -> when (onScreen sv) (drawBody sv)

  drawRadar

  let rscale = 1/scale
      describeTarget target = case target >>= fmap . (,) <*> (system !?) of
        Just (identifier, t)
          | pos <- either Body.actor Character.actor t ^. position_ -> describeIdentifier identifier ++ ": " ++ showEFloat (Just 1) (nu @(Kilo Metres) (distance (pos ^* rscale) (position ^* rscale))) "km"
        _ -> ""

  measure "setLabel" $ setLabel fpsLabel    font (showFFloat (Just 1) (nu @(Milli Seconds) dt) "ms/" <> showFFloat (Just 1) (1/dt) "fps")
  measure "setLabel" $ setLabel targetLabel font (describeTarget target)

  fpsSize <- labelSize fpsLabel
  measure "drawLabel" $ drawLabel fpsLabel    (V2 10 (size^._y - fpsSize^._y - 10)) white Nothing
  measure "drawLabel" $ drawLabel targetLabel (V2 10 10)                            white Nothing
