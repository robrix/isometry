{-# LANGUAGE NamedFieldPuns #-}
module UI.Drawable
( Drawable(..)
, using
, runDrawable
, loadingDrawable
) where

import           Control.Carrier.Reader
import           Control.Effect.Finally
import           Control.Effect.Lift
import           Control.Effect.Trace
import           Data.Functor.I
import           Foreign.Storable (Storable)
import           GL.Array
import           GL.Effect.Check
import           GL.Program
import           GL.Shader.DSL (RShader, Vars)

data Drawable u v o = Drawable
  { program :: Program u v o
  , array   :: Array (v I)
  }

using
  :: ( Has Check sig m
     , Has (Lift IO) sig m
     , Has (Reader a) sig m
     , Vars u
     )
  => (a -> Drawable u v o)
  -> ArrayC v (ProgramC u v o m) b
  -> m b
using getDrawable m = do
  Drawable { program, array } <- asks getDrawable
  use program $ bindArray array m


runDrawable :: (Drawable u v o -> b) -> Drawable u v o -> ReaderC b m a -> m a
runDrawable makeDrawable = runReader . makeDrawable

loadingDrawable :: (Has Check sig m, Has Finally sig m, Has (Lift IO) sig m, Has Trace sig m, Storable (v I), Vars u, Vars v) => (Drawable u v o -> b) -> RShader u v o -> [v I] -> ReaderC b m a -> m a
loadingDrawable drawable shader vertices m = do
  program <- build shader
  (_, array) <- load vertices
  runDrawable drawable Drawable{ program, array } m
