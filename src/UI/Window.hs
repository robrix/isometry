{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module UI.Window
( WindowPixels(..)
, swap
, poll
, input
, size
, ratio
, runSDL
, runWindow
, Window
) where

import           Control.Carrier.Lift
import           Control.Carrier.Reader
import qualified Control.Concurrent.Lift as CC
import qualified Control.Exception.Lift as E
import           Control.Lens ((^.))
import           Control.Monad ((<=<))
import           Control.Monad.IO.Class.Lift
import           Data.Fixed (div')
import           Data.Functor.Identity
import           Data.Text (Text)
import           Foreign.Storable
import           GL.Type as GL
import           GL.Uniform
import           Linear.V2 as Linear
import           Linear.V4 as Linear
import           SDL

newtype WindowPixels a = WindowPixels { getWindowPixels :: a }
  deriving (Conjugate, Enum, Epsilon, Eq, Foldable, Floating, Fractional, Functor, Integral, Num, Ord, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)
  deriving (Additive, Applicative, Metric, Monad) via Identity

swap :: (Has (Lift IO) sig m, Has (Reader Window) sig m) => m ()
swap = ask >>= runLiftIO . glSwapWindow

poll :: Has (Lift IO) sig m => m (Maybe Event)
poll = runLiftIO pollEvent

input :: Has (Lift IO) sig m => (Event -> m ()) -> m ()
input h = go where
  go = poll >>= maybe (pure ()) (const go <=< h)

size :: (Num a, Has (Lift IO) sig m, Has (Reader Window) sig m) => m (V2 a)
size = do
  size <- ask >>= runLiftIO . get . windowSize
  pure (fromIntegral <$> size)

ratio :: (Integral a, Has (Lift IO) sig m, Has (Reader Window) sig m) => m a
ratio = runLiftIO $ do
  window <- ask
  drawableSize <- glGetDrawableSize window
  windowSize <- get (windowSize window)
  pure $! (drawableSize^._y) `div'` (windowSize^._y)


runSDL :: Has (Lift IO) sig m => m a -> m a
runSDL = CC.runInBoundThread . E.bracket_ (runLiftIO initializeAll) (runLiftIO quit)

runWindow :: Has (Lift IO) sig m => Text -> V2 Int -> ReaderC Window m a -> m a
runWindow name size = E.bracket
  (runLiftIO (createWindow name windowConfig))
  (runLiftIO . destroyWindow)
  . flip runReader where
  windowConfig = defaultWindow
    { windowInitialSize     = fromIntegral <$> size
    , windowResizable       = True
    , windowPosition        = Centered
    , windowGraphicsContext = OpenGLContext glConfig
    , windowHighDPI         = True
    }
  glConfig = defaultOpenGL
    { glProfile        = Core Normal 4 1
    , glColorPrecision = V4 8 8 8 8
    }
