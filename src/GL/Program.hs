{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module GL.Program
( Program(..)
, build
, use
, askProgram
, ProgramC(..)
) where

import           Control.Carrier.Reader
import           Control.Carrier.State.Church
import           Control.Effect.Finally
import           Control.Effect.Labelled
import           Control.Effect.Sum
import           Control.Monad.IO.Class.Lift
import           Control.Monad.Trans.Class
import           Data.Foldable (for_)
import           Data.Kind (Type)
import qualified Data.IntMap as IntMap
import           Data.Traversable (for)
import qualified Foreign.C.String.Lift as C
import           GHC.Stack
import           GL.Effect.Check
import           GL.Error
import           GL.Shader
import qualified GL.Shader.DSL as DSL
import           GL.Shader.Vars
import           GL.Uniform
import           Graphics.GL.Core41
import           Graphics.GL.Types

data Program (u :: (Type -> Type) -> Type) (i :: (Type -> Type) -> Type) (o :: (Type -> Type) -> Type) = Program
  { locations :: IntMap.IntMap GLint
  , unProgram :: GLuint
  }


build :: forall u v o m sig . (HasCallStack, Has Check sig m, Has Finally sig m, Has (Lift IO) sig m, Vars u, Vars v) => DSL.RShader u v o -> m (Program u v o)
build p = runLiftIO $ do
  program <- glCreateProgram
  onExit (glDeleteProgram program)
  foldVarsM @v (\ Field { name, location } -> checking $
    C.withCString name (glBindAttribLocation program (fromIntegral location))) defaultVars
  shaders <- for (DSL.shaderSources p) $ \ (type', source) -> do
    shader <- createShader type'
    shader <$ compile source shader

  for_ shaders (glAttachShader program . unShader)
  glLinkProgram program
  for_ shaders (glDetachShader program . unShader)

  checkStatus glGetProgramiv glGetProgramInfoLog Other GL_LINK_STATUS program

  ls <- foldVarsM @u (\ Field{ name, location } -> do
    loc <- checking $ C.withCString name (glGetUniformLocation program)
    pure (IntMap.singleton location loc)) defaultVars

  pure (Program ls program)

use :: Has (Lift IO) sig m => Program u v o -> ProgramC u v o m a -> m a
use (Program ls p) m = do
  sendIO (glUseProgram p)
  runReader (Program ls p) (runProgramC m)


askProgram :: HasLabelled Program (Reader (Program u v o)) sig m => m (Program u v o)
askProgram = runUnderLabel @Program ask


newtype ProgramC (u :: (Type -> Type) -> Type) (v :: (Type -> Type) -> Type) (o :: (Type -> Type) -> Type) m a = ProgramC { runProgramC :: ReaderC (Program u v o) m a }
  deriving (Applicative, Functor, Monad, MonadFail, MonadIO, MonadTrans)

instance (Has Check sig m, Has (Lift IO) sig m, Vars u) => Algebra (State (u Maybe) :+: Labelled Program (Reader (Program u v o)) :+: sig) (ProgramC u v o m) where
  alg hdl sig ctx = case sig of
    L Get      -> pure (makeVars (const Nothing) <$ ctx)
    L (Put s)  -> do
      Program ls prog <- askProgram
      foldVarsM (\ Field{ location, value } ->
        maybe (pure ()) (checking . uniform prog (ls IntMap.! location)) value) s
      pure ctx
    R (L other) -> ProgramC (alg (runProgramC . hdl) (inj (runLabelled other)) ctx)
    R (R other) -> ProgramC (alg (runProgramC . hdl) (R other) ctx)
