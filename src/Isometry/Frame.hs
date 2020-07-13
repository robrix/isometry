{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Isometry.Frame
( runFrame
, frame
) where

import           Control.Carrier.Empty.Church
import           Control.Carrier.Reader
import           Control.Carrier.State.Church
import           Control.Effect.Finally
import           Control.Effect.Labelled
import           Control.Effect.Lens (use)
import           Control.Effect.Lift
import           Control.Effect.Profile
import           Control.Effect.Trace
import           Data.Bin.Index (toInt)
import           Data.Bin.Shape as Shape
import           Data.Coerce (coerce)
import           Data.Unfoldable (tetra)
import           GHC.Stack
import           GL.Effect.Check
import           Isometry.Draw
import qualified Isometry.Draw.Axis as Axis
import qualified Isometry.Draw.Voxel as Voxel
import           Isometry.Input as Input
import           Isometry.Player
import           Isometry.Time
import           Isometry.UI
import           Isometry.View
import           Isometry.Voxel as Voxel
import           Isometry.World
import           Linear.Exts
import qualified UI.Colour as UI
import qualified UI.Window as Window
import           Unit.Length (Metres(..), Semi(..))

runFrame
  :: ( Has Check sig m
     , Has Finally sig m
     , Has (Lift IO) sig m
     , Has Profile sig m
     , Has Trace sig m
     )
  => ReaderC Voxel.Drawable
    (ReaderC Axis.Drawable
    (Labelled World (ReaderC (World S1024 Voxel))
    (StateC Instant
    (EmptyC
    m)))) a
  -> m ()
runFrame
  = evalEmpty
  . (\ m -> now >>= \ start -> evalState start m)
  . (\ m -> do
    world <- measure "build" $ do
      let !world = makeWorld (tetra (\ !v ->
            let !v' = fromIntegral . toInt <$> v
                o = fmap (+ offset) v'
            in Voxel (coerce o) (UI.Colour (normalize v'))))
          !offset = negate (s * 0.5)
          !s = fromIntegral $ Shape.size world
          !factor = 1 / s
          normalize (V3 x y z) = V4 (x * factor) (y * factor) (z * factor) 1

      world <$ trace ("world length: " <> show (length world))
    runReader world m)
  . runLabelled
  . runDrawables


frame
  :: ( Has Check sig m
     , Has Empty sig m
     , Has (Lift IO) sig m
     , Has Profile sig m
     , Has (Reader Axis.Drawable) sig m
     , Has (Reader Voxel.Drawable) sig m
     , Has (Reader UI) sig m
     , Has (Reader Window.Window) sig m
     , Has (State Input) sig m
     , Has (State Instant) sig m
     , Has (State Player) sig m
     , HasLabelled World (Reader (World s Voxel)) sig m
     , HasCallStack
     )
  => m ()
frame = timed $ do
  measure "input" Input.input
  angle <- use angle_

  withView angle $ measure "draw" draw
