{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module UI.Context
( Context
, Pixels(..)
, runContext
, contextSize
, clipToContext
) where

import           Control.Carrier.Reader
import           Control.Effect.Lift
import qualified Control.Exception.Lift as E
import           Control.Monad.IO.Class.Lift
import           Data.Functor.I
import           Data.Functor.Interval
import           Data.Functor.K
import           Foreign.Storable
import           GL.Type as GL
import           GL.Uniform
import           GL.Viewport
import           Graphics.GL.Core41
import           SDL
import           System.Random (Random)
import qualified UI.Window as Window
import           Unit.Length

type Context = GLContext

newtype Pixels a = Pixels { getPixels :: a }
  deriving (Column, Conjugate, Enum, Epsilon, Eq, Foldable, Floating, Fractional, Functor, Integral, Num, Ord, Random, Real, RealFloat, RealFrac, Row, Show, Storable, Traversable, GL.Type, Uniform)
  deriving (Additive, Applicative, Metric, Monad) via I

instance Unit Length Pixels where
  suffix = K ("px"++)

runContext :: (Has (Lift IO) sig m, Has (Reader Window) sig m) => ReaderC Context m a -> m a
runContext = E.bracket
  (ask >>= runLiftIO . glCreateContext)
  (\ c -> runLiftIO (glFinish >> glDeleteContext c))
  . flip runReader


contextSize :: (Has (Lift IO) sig m, Has (Reader Window) sig m) => m (V2 (Pixels Int))
contextSize =
  fmap (Pixels . Window.getCoords) <$> ((.*^) <$> Window.ratio <*> Window.size)


clipToContext :: (Has (Lift IO) sig m, Has (Reader Window) sig m) => m ()
clipToContext = do
  dsize <- contextSize
  viewport $ Interval 0 dsize
  scissor  $ Interval 0 dsize
