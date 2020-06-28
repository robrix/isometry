{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module GL.Buffer
( Buffer(..)
, realloc
, copy
, copyV
, copyMV
, copyA
, copyWith
, Type(..)
, KnownType(..)
, Update(..)
, Usage(..)
, bindBuffer
, askBuffer
) where

import           Control.Carrier.Reader
import           Control.Effect.Labelled
import           Control.Monad.IO.Class.Lift
import           Data.Array.Storable
import           Data.Functor.I
import           Data.Functor.Interval
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import qualified Foreign.Marshal.Array.Lift as A
import           Foreign.Ptr (Ptr, castPtr, nullPtr)
import           Foreign.Storable as S
import           GL.Effect.Check
import           GL.Enum as GL
import           GL.Object
import           Graphics.GL.Core41
import           Graphics.GL.Types
import           Linear.Vector

newtype Buffer (ty :: Type) v = Buffer { unBuffer :: GLuint }

instance Object (Buffer ty v) where
  gen = defaultGenWith glGenBuffers Buffer
  delete = defaultDeleteWith glDeleteBuffers unBuffer

instance KnownType ty => Bind (Buffer ty v) where
  bind = checking . runLiftIO . glBindBuffer (glEnum (typeVal @ty)) . maybe 0 unBuffer

-- FIXME: Store the current size and don’t reallocate when larger.
realloc :: forall ty v m sig . (HasLabelled (Buffer ty) (Reader (Buffer ty v)) sig m, KnownType ty, S.Storable v, Has (Lift IO) sig m) => Int -> Update -> Usage -> m ()
realloc n update usage = askBuffer @ty >> runLiftIO (glBufferData (glEnum (typeVal @ty)) (fromIntegral (n * S.sizeOf @v undefined)) nullPtr (glEnum (Hint update usage)))

copy :: forall ty v m sig . (HasLabelled (Buffer ty) (Reader (Buffer ty v)) sig m, KnownType ty, S.Storable v, Has Check sig m, Has (Lift IO) sig m) => Int -> [v] -> m ()
copy = copyWith @ty A.withArrayLen

copyV :: forall ty v m sig . (HasLabelled (Buffer ty) (Reader (Buffer ty v)) sig m, KnownType ty, S.Storable v, Has Check sig m, Has (Lift IO) sig m) => Int -> V.Vector v -> m ()
copyV = copyWith @ty withVectorLen

withVectorLen :: (Has (Lift IO) sig m, Storable a) => V.Vector a -> (Int -> Ptr a -> m b) -> m b
withVectorLen as with = liftWith $ \ hdl ctx -> V.unsafeWith as (hdl . (<$ ctx) . with (V.length as))

copyMV :: forall ty v m sig . (HasLabelled (Buffer ty) (Reader (Buffer ty v)) sig m, KnownType ty, S.Storable v, Has Check sig m, Has (Lift IO) sig m) => Int -> MV.IOVector v -> m ()
copyMV = copyWith @ty withMutableVectorLen

withMutableVectorLen :: (Has (Lift IO) sig m, Storable a) => MV.IOVector a -> (Int -> Ptr a -> m b) -> m b
withMutableVectorLen as with = liftWith $ \ hdl ctx -> MV.unsafeWith as (hdl . (<$ ctx) . with (MV.length as))

copyA :: forall ty i v m sig . (HasLabelled (Buffer ty) (Reader (Buffer ty v)) sig m, KnownType ty, S.Storable v, Has Check sig m, Has (Lift IO) sig m, Ix i) => Int -> StorableArray i v -> m ()
copyA = copyWith @ty withStorableArrayLen

withStorableArrayLen :: (Has (Lift IO) sig m, Storable a, Ix i) => StorableArray i a -> (Int -> Ptr a -> m b) -> m b
withStorableArrayLen as with = liftWith $ \ hdl ctx -> do
  bounds <- getBounds as
  withStorableArray as (hdl . (<$ ctx) . with (rangeSize bounds))

copyWith :: forall ty t v m sig . (HasLabelled (Buffer ty) (Reader (Buffer ty v)) sig m, KnownType ty, S.Storable v, Has Check sig m, Has (Lift IO) sig m) => (t v -> (Int -> Ptr v -> m ()) -> m ()) -> Int -> t v -> m ()
copyWith with offset vertices = askBuffer @ty >> with vertices
  (\ len -> let i = ((0...len) + point (I offset)) ^* S.sizeOf @v undefined in checking . runLiftIO . glBufferSubData (glEnum (typeVal @ty)) (fromIntegral (inf i)) (fromIntegral (size i)) . castPtr)



data Type
  = Array
  | ElementArray
  | Texture
  deriving (Eq, Ord, Show)

class KnownType (ty :: Type) where
  typeVal :: Type

instance KnownType 'Array where
  typeVal = Array

instance KnownType 'ElementArray where
  typeVal = ElementArray

instance KnownType 'Texture where
  typeVal = Texture

instance GL.Enum Type where
  glEnum = \case
    Array        -> GL_ARRAY_BUFFER
    ElementArray -> GL_ELEMENT_ARRAY_BUFFER
    Texture      -> GL_TEXTURE_BUFFER


data Update
  = Static
  | Dynamic
  | Stream
  deriving (Eq, Ord, Show)

data Usage
  = Draw
  | Read
  | Copy
  deriving (Eq, Ord, Show)

data Hint = Hint Update Usage

instance GL.Enum Hint where
  glEnum = \case
    Hint Static  Draw -> GL_STATIC_DRAW
    Hint Static  Read -> GL_STATIC_READ
    Hint Static  Copy -> GL_STATIC_COPY
    Hint Dynamic Draw -> GL_DYNAMIC_DRAW
    Hint Dynamic Read -> GL_DYNAMIC_READ
    Hint Dynamic Copy -> GL_DYNAMIC_COPY
    Hint Stream  Draw -> GL_STREAM_DRAW
    Hint Stream  Read -> GL_STREAM_READ
    Hint Stream  Copy -> GL_STREAM_COPY


bindBuffer :: (KnownType ty, Has Check sig m, Has (Lift IO) sig m) => Buffer ty v -> BufferC ty v m a -> m a
bindBuffer buffer m = do
  bind (Just buffer)
  a <- runReader buffer (runLabelled m)
  a <$ bind (Nothing `asTypeOf` Just buffer)

askBuffer :: forall ty v m sig . HasLabelled (Buffer ty) (Reader (Buffer ty v)) sig m => m (Buffer ty v)
askBuffer = runUnderLabel @(Buffer ty) ask

type BufferC ty v = Labelled (Buffer ty) (ReaderC (Buffer ty v))
