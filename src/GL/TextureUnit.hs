{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module GL.TextureUnit
( TextureUnit(..)
, Index(..)
, setActiveTexture
) where

import           Control.Monad.IO.Class.Lift
import           Data.Kind (Type)
import           Foreign.Storable
import qualified GL.Type as GL
import           GL.Uniform
import           Graphics.GL.Core41
import           Graphics.GL.Types
import           Linear.V1
import           Linear.V2
import           Linear.V3

newtype TextureUnit (u :: Type -> Type) v = TextureUnit { unTextureUnit :: GLint }
  deriving (Storable)

instance GL.Type (TextureUnit u v) where
  glType = GL_INT

instance Sampler u => Uniform (TextureUnit u v) where
  glslType = glslSamplerType @u
  uniform prog loc = runLiftIO . glProgramUniform1i prog loc . unTextureUnit

class Sampler (u :: Type -> Type) where
  glslSamplerType :: String

instance Sampler V1 where
  glslSamplerType = "sampler1D"

instance Sampler V2 where
  glslSamplerType = "sampler2D"

instance Sampler V3 where
  glslSamplerType = "sampler3D"


newtype Index a = Index { getIndex :: a }
  deriving (Functor)

instance Sampler Index where
  glslSamplerType = "samplerBuffer"


setActiveTexture :: Has (Lift IO) sig m => TextureUnit u v -> m ()
setActiveTexture (TextureUnit i) = runLiftIO $ glActiveTexture (fromIntegral (GL_TEXTURE0 + i))
