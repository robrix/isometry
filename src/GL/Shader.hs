{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
module GL.Shader
( Shader(..)
, Stage(..)
, KnownStage(..)
, createShader
, compile
, checkShader
) where

import           Control.Effect.Finally
import           Control.Effect.Lift
import qualified Foreign.C.String.Lift as C
import qualified Foreign.Marshal.Utils.Lift as U
import           Foreign.Ptr
import           GHC.Stack
import qualified GL.Enum as GL
import           GL.Error
import           Graphics.GL.Core41
import           Graphics.GL.Types

newtype Shader = Shader { unShader :: GLuint }

data Stage
  = Vertex
  | Geometry
  | Fragment
  deriving (Eq, Ord, Show)

instance GL.Enum Stage where
  glEnum = \case
    Vertex   -> GL_VERTEX_SHADER
    Geometry -> GL_GEOMETRY_SHADER
    Fragment -> GL_FRAGMENT_SHADER


class KnownStage (k :: Stage) where
  typeVal :: proxy k -> Stage

instance KnownStage 'Vertex where
  typeVal _ = Vertex

instance KnownStage 'Fragment where
  typeVal _ = Fragment


createShader :: (Has Finally sig m, Has (Lift IO) sig m) => Stage -> m Shader
createShader type' = do
  shader <- sendIO (glCreateShader (GL.glEnum type'))
  Shader shader <$ onExit (sendIO (glDeleteShader shader))

compile :: (Has (Lift IO) sig m, HasCallStack) => String -> Shader -> m ()
compile source (Shader shader) = sendIO $ do
  C.withCString source $ \ source ->
    U.with source $ \ p ->
      glShaderSource shader 1 p nullPtr
  glCompileShader shader
  checkShader source (Shader shader)

checkShader :: (Has (Lift IO) sig m, HasCallStack) => String -> Shader -> m ()
checkShader source = withFrozenCallStack $ sendIO . checkStatus glGetShaderiv glGetShaderInfoLog (Source source) GL_COMPILE_STATUS . unShader
