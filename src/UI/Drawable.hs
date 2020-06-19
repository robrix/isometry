{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
module UI.Drawable
( Drawable(..)
, runDrawable
, loadingDrawable
, Usable(..)
) where

import           Control.Carrier.Reader
import           Control.Effect.Finally
import           Control.Effect.Lift
import           Control.Effect.Trace
import           Data.Functor.I
import           Data.Kind (Type)
import           Foreign.Storable (Storable)
import           GL.Array hiding (Type)
import           GL.Effect.Check
import           GL.Program
import           GL.Shader.DSL (RShader, Vars)

data Drawable u v o = Drawable
  { program :: Program u v o
  , array   :: Array (v I)
  }


runDrawable :: (Drawable u v o -> b) -> Drawable u v o -> ReaderC b m a -> m a
runDrawable makeDrawable = runReader . makeDrawable

loadingDrawable :: (Has Check sig m, Has Finally sig m, Has (Lift IO) sig m, Has Trace sig m, Storable (v I), Vars u, Vars v) => (Drawable u v o -> b) -> RShader u v o -> [v I] -> ReaderC b m a -> m a
loadingDrawable drawable shader vertices m = do
  program <- build shader
  (_, array) <- load vertices
  runDrawable drawable Drawable{ program, array } m


class Usable b where
  type UsableT b (m :: Type -> Type) :: (Type -> Type)

  using
    :: ( Has Check sig m
       , Has (Lift IO) sig m
       , Has (Reader a) sig m
       )
    => (a -> b)
    -> UsableT b m c
    -> m c

instance Usable (Program u v o) where
  type UsableT (Program u v o) m = ProgramC u v o m

  using getProgram m = do
    program <- asks getProgram
    use program m

instance Usable (Array (v I)) where
  type UsableT (Array (v I)) m = ArrayC v m

  using getArray m = do
    array <- asks getArray
    bindArray array m

instance Vars u => Usable (Drawable u v o) where
  type UsableT (Drawable u v o) m = ArrayC v (ProgramC u v o m)

  using getDrawable = using (program . getDrawable) . using (array . getDrawable)
