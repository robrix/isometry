{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
module Isometry.Draw.Voxel
( draw
, runDrawable
, Drawable
) where

import           Control.Carrier.Reader
import           Control.Effect.Finally
import           Control.Effect.Lens ((?=))
import           Control.Effect.Lift
import qualified Control.Effect.Reader.Labelled as Labelled
import           Control.Effect.Trace
import           Control.Lens (Lens', ifoldMap)
import           Control.Monad.IO.Class.Lift
import           Data.Coerce
import           Data.Monoid (Endo(..))
import           Data.Functor.I
import           Data.Functor.Interval hiding (range)
import           Data.Generics.Product.Fields
import           Foreign.Storable
import           Geometry.Transform
import           GHC.Generics
import           GHC.Stack
import           GHC.TypeLits
import           GL.Array
import           GL.Buffer as Buffer
import           GL.Effect.Check
import           GL.Object
import           GL.Shader.DSL as D hiding (get, (.*.), (./.), (^.), _x, _xy, _xz, _y, _yz, _z)
import qualified GL.Shader.DSL as D
import           GL.Texture
import           GL.TextureUnit
import           Graphics.GL.Core41
import           Isometry.Octree as Octree (Size, size, toFraction)
import           Isometry.View as View
import           Isometry.Voxel as Voxel
import           Isometry.World
import           UI.Colour as UI
import qualified UI.Drawable as UI
import           Unit.Length

draw
  :: ( Has Check sig m
     , Has (Lift IO) sig m
     , Has (Reader Drawable) sig m
     , Has (Reader View) sig m
     , Labelled.HasLabelled World (Reader (Octree s Voxel)) sig m
     , HasCallStack
     )
  => m ()
draw = UI.using drawable $ do
  Drawable { originsT, coloursT } <- ask
  v <- ask
  world <- Labelled.ask @World

  setActiveTexture originsU
  bind (Just originsT)

  setActiveTexture coloursU
  bind (Just coloursT)

  matrix_ ?= tmap realToFrac (transformToZoomed v)
  origins_ ?= originsU
  colours_ ?= coloursU
  drawArraysInstanced Triangles (0...length vertices) (length world)


runDrawable
  :: ( Has Check sig m
     , Has Finally sig m
     , Has (Lift IO) sig m
     , Has Trace sig m
     , Labelled.HasLabelled World (Reader (Octree s Voxel)) sig m
     , KnownNat (Size s)
     , HasCallStack
     )
  => ReaderC Drawable m a
  -> m a
runDrawable m = do
  originsT <- gen1 @(Texture 'TextureBuffer)
  coloursT <- gen1 @(Texture 'TextureBuffer)
  originsB <- gen1 @(Buffer 'Buffer.Texture (V4 (Distance Float)))
  coloursB <- gen1 @(Buffer 'Buffer.Texture (UI.Colour Float))

  (origins, colours) <- Labelled.asks @World (unzip . makeVoxels)

  bindBuffer originsB $ do
    realloc @'Buffer.Texture (length origins) Static Read
    copy @'Buffer.Texture 0 (map (\ (V3 x y z) -> V4 x y z 0) origins)

  bindBuffer coloursB $ do
    realloc @'Buffer.Texture (length colours) Static Read
    copy @'Buffer.Texture 0 colours

  setActiveTexture originsU
  bind (Just originsT)
  runLiftIO $ glTexBuffer GL_TEXTURE_BUFFER GL_RGBA32F (unBuffer originsB)

  setActiveTexture coloursU
  bind (Just coloursT)
  runLiftIO $ glTexBuffer GL_TEXTURE_BUFFER GL_RGBA32F (unBuffer coloursB)

  UI.loadingDrawable (\ drawable -> Drawable{ originsT, originsB, coloursT, coloursB, drawable }) shader (coerce vertices) m

makeVoxels :: KnownNat (Size s) => Octree s Voxel -> [(V3 (Distance Float), UI.Colour Float)]
makeVoxels (Octree o) = appEndo (ifoldMap (\ n (Voxel c) -> Endo ((fromIntegral . (+ offset) . fst . toFraction <$> n, c):)) o) []
  where
  offset = negate (Octree.size o `div` 2)


data Drawable = Drawable
  { originsT :: Texture 'TextureBuffer
  , originsB :: Buffer 'Buffer.Texture (V4 (Distance Float))
  , coloursT :: Texture 'TextureBuffer
  , coloursB :: Buffer 'Buffer.Texture (UI.Colour Float)
  , drawable :: UI.Drawable U V Frag
  }

originsU :: TextureUnit Index (V4 (Distance Float))
originsU = TextureUnit 0

coloursU :: TextureUnit Index (UI.Colour Float)
coloursU = TextureUnit 1

vertices :: [V3 (Distance Float)]
vertices = map ((* 0.5) . (+ 1))
  [ -- far
    V3 (-1) (-1) (-1)
  , V3   1  (-1) (-1)
  , V3   1    1  (-1)

  , V3   1    1  (-1)
  , V3 (-1)   1  (-1)
  , V3 (-1) (-1) (-1)

    -- near
  , V3 (-1) (-1)   1
  , V3   1  (-1)   1
  , V3   1    1    1

  , V3   1    1    1
  , V3 (-1)   1    1
  , V3 (-1) (-1)   1

    -- left
  , V3 (-1) (-1) (-1)
  , V3 (-1)   1  (-1)
  , V3 (-1)   1    1

  , V3 (-1)   1    1
  , V3 (-1) (-1)   1
  , V3 (-1) (-1) (-1)

    -- right
  , V3   1  (-1) (-1)
  , V3   1    1  (-1)
  , V3   1    1    1

  , V3   1    1    1
  , V3   1  (-1)   1
  , V3   1  (-1) (-1)

    -- bottom
  , V3 (-1) (-1) (-1)
  , V3   1  (-1) (-1)
  , V3   1  (-1)   1

  , V3   1  (-1)   1
  , V3 (-1) (-1)   1
  , V3 (-1) (-1) (-1)

    -- top
  , V3 (-1)   1  (-1)
  , V3   1    1  (-1)
  , V3   1    1    1

  , V3   1    1    1
  , V3 (-1)   1    1
  , V3 (-1)   1  (-1)
  ]


shader :: D.Shader shader => shader U V Frag
shader
  =   vertex (\ U{ matrix, origins, colours } V{ pos } IF{ colour2 } -> main $ do
    gl_Position .= matrix D.>* ext4 (pos + texelFetch origins (cast gl_InstanceID)D.^.D._xyz) 1
    colour2 .= texelFetch colours (cast gl_InstanceID))

  >>> fragment (\ _ IF{ colour2 } Frag{ fragColour } -> main $
    fragColour .= colour2)


data U v = U
  { matrix  :: v (Transform V4 Float Distance ClipUnits)
  , origins :: v (TextureUnit Index (V4 (Distance Float)))
  , colours :: v (TextureUnit Index (UI.Colour Float))
  }
  deriving (Generic)

instance D.Vars U

matrix_ :: Lens' (U v) (v (Transform V4 Float Distance ClipUnits))
matrix_ = field @"matrix"

origins_ :: Lens' (U v) (v (TextureUnit Index (V4 (Distance Float))))
origins_ = field @"origins"

colours_ :: Lens' (U v) (v (TextureUnit Index (UI.Colour Float)))
colours_ = field @"colours"


newtype V v = V { pos :: v (V3 (Distance Float)) }
  deriving (Generic)

instance D.Vars V

deriving via Fields V instance Storable (V I)


newtype IF v = IF
  { colour2 :: v (Colour Float)
  }
  deriving (Generic)

instance D.Vars IF
