{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Isometry.Frame
( runFrame
) where

import           Control.Carrier.Empty.Church
import           Control.Carrier.Reader
import           Control.Carrier.State.Church
import           Control.Effect.Finally
import           Control.Effect.Labelled
import           Control.Effect.Lift
import           Control.Effect.Profile
import           Control.Effect.Trace
import           Control.Lens ((&), (.~))
import           Data.Bin.Index (toInt)
import           Data.Bin.Shape as Shape
import           Data.Unfoldable (tetra)
import           GL.Effect.Check
import qualified Isometry.Draw.Axis as Axis
import qualified Isometry.Draw.Voxel as Voxel
import           Isometry.Time
import           Isometry.Voxel as Voxel
import           Isometry.World
import           Linear.Exts
import qualified UI.Colour as UI

runFrame
  :: ( Has Check sig m
     , Has Finally sig m
     , Has (Lift IO) sig m
     , Has Profile sig m
     , Has Trace sig m
     )
  => ReaderC Voxel.Drawable
    (ReaderC Axis.Drawable
    (Labelled World (ReaderC (World S128 Voxel))
    (StateC Instant
    (EmptyC
    m)))) a
  -> m ()
runFrame
  = evalEmpty
  . (\ m -> now >>= \ start -> evalState start m)
  . (\ m -> do
    world <- measure "build" $ do
      let world = makeWorld (tetra (\ v ->
            let o = fmap (fromIntegral . (+ offset) . toInt) v
            in Voxel o 0 & UI.colour_ .~ UI.Colour (ext ((/ fromIntegral s) . fromIntegral . toInt <$> v) 1)))
          !offset = negate (s `div` 2)
          !s = Shape.size world

      world <$ trace ("world length: " <> show (length world))
    runReader world m)
  . runLabelled
  . Axis.runDrawable
  . Voxel.runDrawable
