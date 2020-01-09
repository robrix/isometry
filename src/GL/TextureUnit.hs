{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GL.TextureUnit
( TextureUnit(..)
, setActiveTexture
) where

import Control.Monad.IO.Class.Lift
import Foreign.Storable
import GL.Type as GL
import GL.Uniform
import Graphics.GL.Core41
import Graphics.GL.Types

newtype TextureUnit = TextureUnit { unTextureUnit :: GLint }
  deriving (Storable)

instance GL.Type TextureUnit where
  glType = GL_INT

instance Uniform TextureUnit where
  glslType = "sampler2D"
  uniform prog loc = runLiftIO . glProgramUniform1i prog loc . unTextureUnit


setActiveTexture :: Has (Lift IO) sig m => TextureUnit -> m ()
setActiveTexture (TextureUnit i) = runLiftIO $ glActiveTexture (fromIntegral (GL_TEXTURE0 + i))
