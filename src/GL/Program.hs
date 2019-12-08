{-# LANGUAGE AllowAmbiguousTypes, DataKinds, FlexibleInstances, FunctionalDependencies, KindSignatures, ScopedTypeVariables, TypeApplications, TypeOperators, UndecidableInstances #-}
module GL.Program
( Program(..)
, (:::)(..)
, createProgram
, useProgram
, withProgram
, withLinkedProgram
, link
, withBuiltProgram
, checkProgram
  -- * Uniforms
, Var(..)
, setUniformValue
, HasUniform(..)
) where

import Control.Effect.Finally
import qualified Control.Exception.Lift as E
import Control.Monad.IO.Class.Lift
import Data.Foldable (for_)
import Data.Proxy
import qualified Foreign.C.String.Lift as C
import GHC.Stack
import GHC.TypeLits
import GL.Error
import GL.Shader
import GL.Uniform
import Graphics.GL.Core41
import Graphics.GL.Types

newtype Program (ty :: [Symbol ::: *]) = Program { unProgram :: GLuint }
  deriving (Eq, Ord, Show)

data a ::: b = a ::: b
  deriving (Eq, Ord, Show)

infix 9 :::

createProgram :: (Has Finally sig m, Has (Lift IO) sig m) => m (Program ty)
createProgram = do
  program <- runLiftIO glCreateProgram
  Program program <$ onExit (runLiftIO (glDeleteProgram program))

useProgram :: Has (Lift IO) sig m => Program ty -> m ()
useProgram = runLiftIO . glUseProgram . unProgram

withProgram :: Has (Lift IO) sig m => (Program ty -> m a) -> m a
withProgram = E.bracket
  (runLiftIO (Program <$> glCreateProgram))
  (runLiftIO . glDeleteProgram . unProgram)

withLinkedProgram :: (Has (Lift IO) sig m, HasCallStack) => [Shader] -> (Program ty -> m a) -> m a
withLinkedProgram shaders body = withProgram $ \ program -> do
  link shaders program
  body program

link :: (Has (Lift IO) sig m, HasCallStack) => [Shader] -> Program ty -> m ()
link shaders (Program program) = runLiftIO $ do
  for_ shaders (glAttachShader program . unShader)
  glLinkProgram program
  for_ shaders (glDetachShader program . unShader)
  checkProgram (Program program)

withBuiltProgram :: (Has (Lift IO) sig m, HasCallStack) => [(ShaderType, String)] -> (Program ty -> m a) -> m a
withBuiltProgram sources body = withCompiledShaders sources (`withLinkedProgram` body)


checkProgram :: (Has (Lift IO) sig m, HasCallStack) => Program ty -> m ()
checkProgram = runLiftIO . checkStatus glGetProgramiv glGetProgramInfoLog Other GL_LINK_STATUS . unProgram


newtype Var (name :: Symbol) t = Var t

setUniformValue :: forall name t ty m sig . (HasUniform name t ty, Has (Lift IO) sig m, HasCallStack) => Program ty -> Var name t -> m ()
setUniformValue program (Var v) = do
  location <- checkingGLError . runLiftIO $ C.withCString (symbolVal (Proxy :: Proxy name)) (glGetUniformLocation (unProgram program))
  checkingGLError $ uniform location v


class (KnownSymbol sym, Uniform t) => HasUniform (sym :: Symbol) t (tys :: [Symbol ::: *]) | sym tys -> t where
  uniformLocation :: GLint

instance {-# OVERLAPPABLE #-} (KnownSymbol sym, Uniform t) => HasUniform sym t (sym '::: t ': tys) where
  uniformLocation = 0

instance {-# OVERLAPPABLE #-} HasUniform sym t tys => HasUniform sym t (ty ': tys) where
  uniformLocation = 1 + uniformLocation @sym @t @tys
