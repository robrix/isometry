{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
module Isometry.Draw.Voxel
( draw
, visibleIndices
, visible
, foldN
, runDrawable
, Drawable
, corners
, indices
) where

import           Control.Carrier.Reader
import           Control.Effect.Finally
import           Control.Effect.Lens ((?=))
import           Control.Effect.Lift
import           Control.Effect.Profile
import qualified Control.Effect.Reader.Labelled as Labelled
import           Control.Effect.Trace
import           Control.Lens (Lens', over, (&), (+~))
import           Data.Bin.Octree (Octree(..), withOctreeLen2)
import qualified Data.Bin.Shape as Shape
import           Data.Coerce
import           Data.Foldable (for_)
import           Data.Functor.I
import           Data.Functor.Interval hiding (transform)
import           Data.Generics.Product.Fields
import qualified Data.IntervalSet as I
import           Data.Word
import           Foreign.Storable.Lift
import           Geometry.Transform
import           GHC.Generics
import           GHC.Stack
import           GHC.TypeLits
import           GL.Array
import           GL.Buffer as Buffer
import           GL.Effect.Check
import           GL.Object
import           GL.Program
import           GL.Shader.DSL as D hiding (get, (.*.), (./.), (^.), _x, _xy, _xyz, _xz, _y, _yz, _z)
import qualified GL.Shader.DSL as D
import           GL.Texture
import           GL.TextureUnit
import           Graphics.GL.Core41
import           Isometry.View as View
import           Isometry.Voxel as Voxel
import           Isometry.World
import           Linear.Exts hiding (E)
import           UI.Colour as UI
import qualified UI.Drawable as UI
import           Unit.Length

draw
  :: ( Has Check sig m
     , Has (Lift IO) sig m
     , Has (Reader Drawable) sig m
     , Has (Reader View) sig m
     , Labelled.HasLabelled World (Reader (World s Voxel)) sig m
     , HasCallStack
     , KnownNat (Shape.Size s)
     )
  => m ()
draw = UI.using drawable $ do
  Drawable { originsT, coloursT, indicesB } <- ask
  t <- asks transform
  World world <- Labelled.ask @World

  setActiveTexture originsU
  bind (Just originsT)

  setActiveTexture coloursU
  bind (Just coloursT)

  matrix_  ?= t
  origins_ ?= originsU
  colours_ ?= coloursU
  let is = I.toList (visibleIndices t world)

  bindBuffer indicesB . for_ is $ \ i -> do
    offset_ ?= getI (inf i)
    drawElementsInstanced Triangles indicesI (getI (diameter i))

visibleIndices :: KnownNat (Shape.Size s) => Transform V4 Float Distance ClipUnits -> Octree s a -> I.IntervalSet Int
visibleIndices t o = snd (foldN shouldRecur go (0, I.singleton (0...length o)) o)
  where
  shouldRecur n cube = n < 3 && visible (realToFrac <$> cube) t
  go :: Interval V3 Int -> Octree s' a -> (Int, I.IntervalSet Int) -> (Int, I.IntervalSet Int)
  go cube o (prev, indices) = (next, if visible (realToFrac <$> cube) t then indices else I.delete i indices)
    where
    !next = prev + length o
    !i = prev...pred next

visible :: Interval V3 (Distance Float) -> Transform V4 Float Distance ClipUnits -> Bool
visible i t = any (`intersects` (-1...1 :: Interval I (ClipUnits Float))) (liftI (...) (Interval inf' sup'))
  where
  !inf' = over (extended 1) (apply t) (inf i)
  !sup' = over (extended 1) (apply t) (sup i)

foldN
  :: forall s a b
  .  KnownNat (Shape.Size s)
  => (Int -> Interval V3 Int -> Bool)
  -> (forall s . Interval V3 Int -> Octree s a -> b -> b)
  -> b
  -> Octree s a
  -> b
foldN r f z o = go 0 s (pure (-s `div` 2)) o z
  where
  !s = Shape.size o
  go :: Int -> Int -> V3 Int -> Octree s' a -> b -> b
  go n !s !o t = case t of
    _ | not (r n (Interval o (o + pure s))) -> id
    E   -> id
    L _ -> f (Interval o (o + pure s)) t
    B _ lbf rbf ltf rtf lbn rbn ltn rtn
      | let !s' = s `div` 2
            !n' = n + 1
            go' = go n' s'
      -> f (Interval o (o + pure s)) t
      .  go' o                    lbf . go' (o & _x   +~      s') rbf
      .  go' (o & _y  +~      s') ltf . go' (o & _xy  +~ pure s') rtf
      .  go' (o & _z  +~      s') lbn . go' (o & _xz  +~ pure s') rbn
      .  go' (o & _yz +~ pure s') ltn . go' (o & _xyz +~ pure s') rtn


runDrawable
  :: ( Has Check sig m
     , Has Finally sig m
     , Has (Lift IO) sig m
     , Has Profile sig m
     , Has Trace sig m
     , Labelled.HasLabelled World (Reader (World s Voxel)) sig m
     , HasCallStack
     )
  => ReaderC Drawable m a
  -> m a
runDrawable m = do
  originsT <- gen1 @(Texture 'TextureBuffer)
  coloursT <- gen1 @(Texture 'TextureBuffer)
  originsB <- gen1 @(Buffer 'Buffer.Texture (V3 (Distance Float)))
  coloursB <- gen1 @(Buffer 'Buffer.Texture (UI.Colour Float))
  indicesB <- gen1 @(Buffer 'ElementArray Word32)
  world <- Labelled.asks @World voxels

  measure "alloc & copy octree" $
    withOctreeLen2 world ((,) <$> origin <*> Voxel.colour) $ \ len origins colours -> do
      measure "alloc & copy origins" . bindBuffer originsB $ do
        realloc @'Buffer.Texture len Static Read
        copyPtr @'Buffer.Texture (0...len) origins

      measure "alloc & copy colours" . bindBuffer coloursB $ do
        realloc @'Buffer.Texture len Static Read
        copyPtr @'Buffer.Texture (0...len) colours

  measure "alloc & copy indices" . bindBuffer indicesB $ do
    realloc @'Buffer.ElementArray (length indices) Static Read
    copy @'Buffer.ElementArray 0 indices

  setActiveTexture originsU
  bind (Just originsT)
  sendIO $ glTexBuffer GL_TEXTURE_BUFFER GL_RGB32F (unBuffer originsB)

  setActiveTexture coloursU
  bind (Just coloursT)
  sendIO $ glTexBuffer GL_TEXTURE_BUFFER GL_RGBA32F (unBuffer coloursB)

  program <- build shader
  (_, array) <- load (coerce corners)

  runReader Drawable{ originsT, coloursT, indicesB, drawable = UI.Drawable{ program, array } } m


data Drawable = Drawable
  { originsT :: Texture 'TextureBuffer
  , coloursT :: Texture 'TextureBuffer
  , indicesB :: Buffer 'ElementArray Word32
  , drawable :: UI.Drawable U V Frag
  }

originsU :: TextureUnit Index (V3 (Distance Float))
originsU = TextureUnit 0

coloursU :: TextureUnit Index (UI.Colour Float)
coloursU = TextureUnit 1

corners :: [V3 (Distance Float)]
corners = V3 <$> [0, 1] <*> [0, 1] <*> [0, 1]

indices :: [Word32]
indices =
  [ -- far
    0
  , 4
  , 6
  , 6
  , 2
  , 0
    -- near
  , 1
  , 3
  , 7
  , 7
  , 5
  , 1
    -- left
  , 0
  , 2
  , 3
  , 3
  , 1
  , 0
    -- right
  , 4
  , 5
  , 7
  , 7
  , 6
  , 4
    -- bottom
  , 0
  , 1
  , 5
  , 5
  , 4
  , 0
    -- top
  , 2
  , 6
  , 7
  , 7
  , 3
  , 2
  ]

indicesI :: Interval I Int
indicesI = 0...length indices


shader :: D.Shader shader => shader U V Frag
shader
  =   vertex (\ U{ matrix, origins, offset } V{ pos } IF{ colour } -> main $ do
    gl_Position .= matrix D.>* ext4 (pos + texelFetch origins (cast (gl_InstanceID + offset))D.^.D._xyz) 1
    colour .= cast (gl_InstanceID + offset))

  >>> fragment (\ U{ colours } IF{ colour } Frag{ fragColour } -> main $
    fragColour .= coerce @(_ (V4 Float)) (texelFetch colours (cast colour)))


data U v = U
  { matrix  :: v (Transform V4 Float Distance ClipUnits)
  , origins :: v (TextureUnit Index (V3 (Distance Float)))
  , colours :: v (TextureUnit Index (UI.Colour Float))
  , offset  :: v Int
  }
  deriving (Generic)

instance D.Vars U

matrix_ :: Lens' (U v) (v (Transform V4 Float Distance ClipUnits))
matrix_ = field @"matrix"

origins_ :: Lens' (U v) (v (TextureUnit Index (V3 (Distance Float))))
origins_ = field @"origins"

colours_ :: Lens' (U v) (v (TextureUnit Index (UI.Colour Float)))
colours_ = field @"colours"

offset_ :: Lens' (U v) (v Int)
offset_ = field @"offset"


newtype V v = V { pos :: v (V3 (Distance Float)) }
  deriving (Generic)

instance D.Vars V

deriving via Fields V instance Storable (V I)


newtype IF v = IF
  { colour :: v Float
  }
  deriving (Generic)

instance D.Vars IF
