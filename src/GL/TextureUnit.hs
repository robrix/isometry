{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
module GL.TextureUnit
( TextureUnit(..)
, setActiveTexture
) where

import           Control.Monad.IO.Class.Lift
import           Data.Kind (Type)
import           Foreign.Storable
import qualified GL.Type as GL
import           GL.Uniform
import           Graphics.GL.Core41
import           Graphics.GL.Types

newtype TextureUnit (u :: Type -> Type) (v :: Type -> Type) = TextureUnit { unTextureUnit :: GLint }
  deriving (Storable)

instance GL.Type (TextureUnit u v) where
  glType = GL_INT

instance Uniform (TextureUnit u v) where
  glslType = "sampler2D"
  uniform prog loc = runLiftIO . glProgramUniform1i prog loc . unTextureUnit


setActiveTexture :: Has (Lift IO) sig m => TextureUnit u v -> m ()
setActiveTexture (TextureUnit i) = runLiftIO $ glActiveTexture (fromIntegral (GL_TEXTURE0 + i))
