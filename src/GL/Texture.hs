{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module GL.Texture
( Texture(..)
, Type(..)
, KnownType(..)
, InternalFormat(..)
, PixelFormat(..)
, PixelType(..)
, setImageFormat
, FilterType(..)
, Filter(..)
, WrapCoord(..)
, Wrap(..)
, setParameter
, Parameter
) where

import Control.Monad.IO.Class.Lift
import Data.Proxy
import Foreign.Ptr (nullPtr)
import GHC.Stack
import GL.Enum as GL
import GL.Effect.Check
import GL.Object
import Graphics.GL.Core41
import Graphics.GL.Types
import Linear.V2

newtype Texture (ty :: Type) = Texture { unTexture :: GLuint }

instance Object (Texture ty) where
  gen = defaultGenWith glGenTextures Texture
  delete = defaultDeleteWith glDeleteTextures unTexture

instance KnownType ty => Bind (Texture ty) where
  bind = checking . runLiftIO . glBindTexture (glEnum (typeVal (Proxy :: Proxy ty))) . maybe 0 unTexture


data Type
  = Texture2D
  deriving (Eq, Ord, Show)

class KnownType (ty :: Type) where
  typeVal :: proxy ty -> Type

instance KnownType 'Texture2D where
  typeVal _ = Texture2D

instance GL.Enum Type where
  glEnum = \case
    Texture2D -> GL_TEXTURE_2D


data InternalFormat
  = RGBA8

instance GL.Enum InternalFormat where
  glEnum = \case
    RGBA8 -> GL_RGBA8

data PixelFormat
  = RGBA

instance GL.Enum PixelFormat where
  glEnum = \case
    RGBA -> GL_RGBA

data PixelType
  = Packed8888 Bool

instance GL.Enum PixelType where
  glEnum = \case
    Packed8888 False -> GL_UNSIGNED_INT_8_8_8_8
    Packed8888 True  -> GL_UNSIGNED_INT_8_8_8_8_REV

setImageFormat :: (HasCallStack, Has Check sig m, Has (Lift IO) sig m) => Type -> InternalFormat -> V2 Int -> PixelFormat -> PixelType -> m ()
setImageFormat target internalFormat (V2 width height) pixelFormat pixelType = checking . runLiftIO $ glTexImage2D (glEnum target) 0 (fromIntegral (glEnum internalFormat)) (fromIntegral width) (fromIntegral height) 0 (glEnum pixelFormat) (glEnum pixelType) nullPtr


data FilterType = MinFilter | MagFilter

instance GL.Enum FilterType where
  glEnum = \case
    MinFilter -> GL_TEXTURE_MIN_FILTER
    MagFilter -> GL_TEXTURE_MAG_FILTER


data Filter = Nearest | Linear

instance GL.Enum Filter where
  glEnum = \case
    Nearest -> GL_NEAREST
    Linear  -> GL_LINEAR


data WrapCoord = WrapR | WrapS | WrapT

instance GL.Enum WrapCoord where
  glEnum = \case
    WrapR -> GL_TEXTURE_WRAP_R
    WrapS -> GL_TEXTURE_WRAP_S
    WrapT -> GL_TEXTURE_WRAP_T


data Wrap
  = Repeat
  | MirroredRepeat
  | ClampToEdge
  | ClampToBorder

instance GL.Enum Wrap where
  glEnum = \case
    Repeat         -> GL_REPEAT
    MirroredRepeat -> GL_MIRRORED_REPEAT
    ClampToEdge    -> GL_CLAMP_TO_EDGE
    ClampToBorder  -> GL_CLAMP_TO_BORDER


setParameter :: (Parameter val param, Has Check sig m, Has (Lift IO) sig m) => Type -> param -> val -> m ()
setParameter target param = checking . runLiftIO . glTexParameteri (glEnum target) (glEnum param) . fromIntegral . glEnum

class (GL.Enum param, GL.Enum val) => Parameter val param | param -> val

instance Parameter Filter FilterType
instance Parameter Wrap WrapCoord
