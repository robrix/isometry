{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}
module Isometry.View
( View(..)
, withView
  -- * Transforms
, transformToWindowSize
  -- * Re-exports
, module Geometry.Transform
) where

import Control.Carrier.Reader
import Control.Effect.Lift
import Control.Lens ((&), (.~))
import Data.Functor.I
import Geometry.Transform
import GL.Shader.DSL (ClipUnits(..))
import Isometry.World
import Linear.Exts
import UI.Window as Window
import Unit.Algebra
import Unit.Length

data View = View
  { ratio     :: I Int    -- ^ Ratio of window pixels per context pixel.
  , size      :: V2 (Window.Coords Int)
  , zoom      :: I Double
  , scale     :: (Window.Coords :/: Distance) Double
  , transform :: Transform V4 Float Distance ClipUnits
  }

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
      focus = 0 :: V3 (Distance Double)
      -- how many pixels to draw something / one semimetre across
      scale = Window.Coords 5 ./. Semi (Metres 1)
      transform
        =   tmap realToFrac
        $   transformToWindowSize size
        <<< mkScale (pure scale)
        <<< mkTranslation (negated focus)
        <<< mkRotation
            ( axisAngle (unit _x) (pi/4)
            * axisAngle (unit _y) angle)
        <<< mkScale (pure zoom)

  runReader View{ ratio, size, zoom, scale, transform } m


transformToWindowSize :: V2 (Window.Coords Int) -> Transform V4 Double Window.Coords ClipUnits
transformToWindowSize size
  -- NB: we *always* use 2/size, rather than ratio/size, because clip space always extends from -1...1, i.e. it always has diameter 2. this is true irrespective of the DPI ratio.
  = mkScale (pure 1 & _xy .~ ClipUnits 2 ./^ (fmap fromIntegral <$> size) & _z .~ -1/100000)
