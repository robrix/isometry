module GL.Viewport
( viewport
, scissor
) where

import Control.Effect.Lift
import Data.Functor.Interval
import Graphics.GL.Core41
import Linear.V2

viewport :: (Integral a, Has (Lift IO) sig m) => Interval V2 a -> m ()
viewport i = sendIO (glViewport x y w h) where
  V2 x y = fromIntegral <$> inf      i
  V2 w h = fromIntegral <$> diameter i

scissor :: (Integral a, Has (Lift IO) sig m) => Interval V2 a -> m ()
scissor i = sendIO (glScissor x y w h) where
  V2 x y = fromIntegral <$> inf      i
  V2 w h = fromIntegral <$> diameter i
