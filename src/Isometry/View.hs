{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Isometry.View
( View(..)
, contextSize
, lengthToWindowPixels
, withView
  -- * Transforms
, transformToWindow
, transformToZoomed
, transformToWorld
  -- * Viewport
, clipTo
  -- * Re-exports
, module Geometry.Transform
) where

import Control.Carrier.Reader
import Control.Effect.Lift
import Control.Lens ((&), (.~))
import Data.Functor.I
import Data.Functor.Interval
import Geometry.Transform
import GL.Shader.DSL (ClipUnits(..))
import GL.Viewport
import Linear.Exts
import UI.Context as Context
import UI.Window as Window
import Unit.Algebra
import Unit.Length

type Distance = Metres

data View = View
  { ratio :: I Int    -- ^ Ratio of window pixels per context pixel.
  , size  :: V2 (Window.Coords Int)
  , zoom  :: I Double
  , scale :: (Window.Coords :/: Distance) Double
  , focus :: V2 (Distance Double)
  , angle :: I Double
  }

contextSize :: View -> V2 (Context.Pixels Int)
contextSize View{ ratio, size } = Context.Pixels . Window.getCoords <$> ratio .*^ size

lengthToWindowPixels :: View -> (Window.Coords :/: Distance) Double
lengthToWindowPixels View{ zoom, scale } = scale .*. zoom

withView
  :: ( Has (Lift IO) sig m
     , Has (Reader Window.Window) sig m
     )
  => I Double
  -> ReaderC View m a
  -> m a
withView angle m = do
  ratio <- Window.ratio
  size  <- Window.size

  let zoom = 1
      focus = 0
      -- how many pixels to draw something / one metre across
      scale = Window.Coords 10 ./. Metres 1

  runReader View{ ratio, size, zoom, scale, focus, angle } m


transformToWindow :: View -> Transform V4 Double Window.Coords ClipUnits
transformToWindow View{ size }
  -- NB: we *always* use 2/size, rather than ratio/size, because clip space always extends from -1...1, i.e. it always has diameter 2. this is true irrespective of the DPI ratio.
  = mkScale (pure 1 & _xy .~ ClipUnits 2 ./^ (fmap fromIntegral <$> size) & _z .~ 1/100)

-- fixme: shouldn’t we apply the zoom factor *after* the rest?
transformToZoomed :: View -> Transform V4 Double Window.Coords ClipUnits
transformToZoomed view@View{ zoom }
  =   transformToWindow view
  <<< mkScale (pure zoom)

transformToWorld :: View -> Transform V4 Double Distance ClipUnits
transformToWorld view@View{ scale, focus, angle }
  =   transformToZoomed view
  <<< mkScale (pure scale)
  <<< mkTranslation (ext (negated focus) 0)
  <<< mkRotation
      ( axisAngle (unit _x) (pi/4)
      * axisAngle (unit _y) angle)


clipTo :: Has (Lift IO) sig m => View -> m ()
clipTo view = do
  let dsize = contextSize view
  viewport $ Interval 0 dsize
  scissor  $ Interval 0 dsize
