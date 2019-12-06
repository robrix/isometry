{-# LANGUAGE LambdaCase, ScopedTypeVariables, TypeApplications #-}
module GL.Array
( Array(..)
, withArray
, bindArray
, Mode(..)
, Range(..)
, drawArrays
) where

import Data.Coerce
import Data.Foldable (toList)
import Data.Proxy
import qualified Foreign.Marshal.Array as A
import Foreign.Ptr
import qualified Foreign.Storable as S
import GL.Buffer
import GL.Error
import GL.Object
import GL.Scalar
import Graphics.GL.Core41
import Graphics.GL.Types

newtype Array n = Array { unArray :: GLuint }

withArray :: forall v n a. (Foldable v, Scalar n) => [v n] -> (Array n -> IO a) -> IO a
withArray vertices body = with $ \ buffer -> do
  glBindBuffer GL_ARRAY_BUFFER (unBuffer buffer)
  A.withArrayLen (vertices >>= toList) $ \ n p ->
    glBufferData GL_ARRAY_BUFFER (fromIntegral (n * S.sizeOf @n 0)) (castPtr p) GL_STATIC_DRAW
  with $ \ array -> do
    bindArray array
    glEnableVertexAttribArray 0
    glVertexAttribPointer 0 (fromIntegral (length (head vertices))) (glType (Proxy @n)) GL_FALSE 0 nullPtr
    body array

bindArray :: Array n -> IO ()
bindArray = checkingGLError . glBindVertexArray . unArray

instance Object (Array n) where
  construct = coerce
  gen = coerce (glGenVertexArrays @IO)
  delete = coerce (glDeleteVertexArrays @IO)


data Mode
  = Lines
  | LineStrip
  | LineLoop
  | TriangleStrip
  | Triangles
  deriving (Eq, Show)

modeToGLEnum :: Mode -> GLenum
modeToGLEnum = \case
  Lines         -> GL_LINES
  LineStrip     -> GL_LINE_STRIP
  LineLoop      -> GL_LINE_LOOP
  TriangleStrip -> GL_TRIANGLE_STRIP
  Triangles     -> GL_TRIANGLES

data Range = Range
  { rangeFrom  :: {-# UNPACK #-} !Int
  , rangeCount :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Show)

drawArrays :: Mode -> Range -> IO ()
drawArrays mode (Range from count) = checkingGLError $ glDrawArrays (modeToGLEnum mode) (fromIntegral from) (fromIntegral count)
