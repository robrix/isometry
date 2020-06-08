{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Starlight.View
( View(..)
, contextSize
, lengthToWindowPixels
, withView
  -- * Transforms
, transformToWindow
, transformToZoomed
, transformToSystem
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
  { ratio     :: I Int    -- ^ Ratio of window pixels per context pixel.
  , size      :: V2 (Window.Coords Int)
  , zoom      :: I Double
  , scale     :: (Window.Coords :/: Distance) Double
  , focus     :: V2 (Distance Double)
  }

contextSize :: View -> V2 (Context.Pixels Int)
contextSize View{ ratio, size } = Context.Pixels . Window.getCoords <$> ratio .*^ size

lengthToWindowPixels :: View -> (Window.Coords :/: Distance) Double
lengthToWindowPixels View{ zoom, scale } = scale .*. zoom

withView
  :: ( Has (Lift IO) sig m
     , Has (Reader Window.Window) sig m
     )
  => ReaderC View m a
  -> m a
withView m = do
  ratio <- Window.ratio
  size  <- Window.size

  let zoom = 1
      focus = 0
      -- how many pixels to draw something / the radius of the sun
      scale = Window.Coords 695_500 ./. convert @(Kilo Metres) @Distance 695_500
      -- FIXME: this is really stupid; there *has* to be a better way to say “I want a 500 m ship to be 30 px long” or w/e

  runReader View{ ratio, size, zoom, scale, focus } m


transformToWindow :: View -> Transform V4 Double Window.Coords ClipUnits
transformToWindow View{ size }
  -- NB: we *always* use 2/size, rather than ratio/size, because clip space always extends from -1...1, i.e. it always has diameter 2. this is true irrespective of the DPI ratio.
  = mkScale (pure 1 & _xy .~ ClipUnits 2 ./^ (fmap fromIntegral <$> size))

transformToZoomed :: View -> Transform V4 Double Window.Coords ClipUnits
transformToZoomed view@View{ zoom }
  =   transformToWindow view
  <<< mkScale (pure zoom)

transformToSystem :: View -> Transform V4 Double Distance ClipUnits
transformToSystem view@View{ scale, focus }
  =   transformToZoomed view
  <<< mkScale (pure scale)
  <<< mkTranslation (ext (negated focus) 0)


clipTo :: Has (Lift IO) sig m => View -> m ()
clipTo view = do
  let dsize = contextSize view
  viewport $ Interval 0 dsize
  scissor  $ Interval 0 dsize
