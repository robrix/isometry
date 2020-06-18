{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
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
import           Control.Effect.Trace
import           Control.Lens (Lens', (&), (+~))
import           Data.Functor.I
import           Data.Functor.Interval hiding (range)
import           Data.Generics.Product.Fields
import           Foreign.Storable
import           Geometry.Transform
import           GHC.Generics
import           GHC.TypeLits
import           GL.Array
import           GL.Effect.Check
import           GL.Shader.DSL as D hiding (get, (.*.), (./.), (^.), _x, _xy, _xz, _y, _yz, _z)
import           Isometry.Octree as Octree (B(..), Oct(..), Shape(..), Size, size)
import           Isometry.View as View
import           Linear.V3
import           UI.Colour as UI
import qualified UI.Drawable as UI
import           Unit.Length

draw
  :: ( Has Check sig m
     , Has (Lift IO) sig m
     , Has (Reader Drawable) sig m
     , Has (Reader (Interval I Int)) sig m
     , Has (Reader View) sig m
     )
  => m ()
draw = UI.using getDrawable $ do
  v <- ask
  matrix_ ?= tmap realToFrac (transformToWorld v)
  drawArrays Triangles =<< ask


-- fixme: run with some initial size & draw with an octree from a Reader var
runDrawable
  :: ( Has Check sig m
     , Has Finally sig m
     , Has (Lift IO) sig m
     , Has Trace sig m
     )
  => ReaderC Drawable (ReaderC (Interval I Int) m) a
  -> m a
runDrawable = runReader (0...length vertices) . UI.loadingDrawable Drawable shader vertices
  where vertices = makeVertices octree6

makeVertices :: KnownNat (Size s) => B s Oct (UI.Colour Float) -> [V I]
makeVertices o = go (-pure (d0 `div` 2)) d0 o
  where
  d0 = Octree.size o
  go
    :: V3 Integer
    -> Integer
    -> B s Oct (UI.Colour Float)
    -> [V I]
  go n d = \case
    E -> []
    L c ->  map (\ v -> V (I (fmap fromIntegral n + v)) (I c)) vertices
    B (Oct bln ry1n
           tln ry2n
           blf ry1f
           tlf ry2f) -> go n d' bln <> go (n & _x +~ d') d' ry1n
                     <> go (n & _y +~ d') d' tln <> go (n & _xy +~ pure d') d' ry2n
                     <> go (n & _z +~ d') d' blf <> go (n & _xz +~ pure d') d' ry1f
                     <> go (n & _yz +~ pure d') d' tlf <> go (n + pure d') d' ry2f
    where
    d' = d `div` 2


newtype Drawable = Drawable { getDrawable :: UI.Drawable U V Frag }


vertices :: [V3 (Metres Float)]
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
  =   vertex (\ U{ matrix } V{ pos, colour } IF{ colour2 } -> main $ do
    gl_Position .= matrix D.>* ext4 pos 1
    colour2 .= colour)

  >>> fragment (\ _ IF{ colour2 } Frag{ fragColour } -> main $
    fragColour .= colour2)


newtype U v = U
  { matrix :: v (Transform V4 Float Metres ClipUnits)
  }
  deriving (Generic)

instance D.Vars U

matrix_ :: Lens' (U v) (v (Transform V4 Float Metres ClipUnits))
matrix_ = field @"matrix"


data V v = V
  { pos    :: v (V3 (Metres Float))
  , colour :: v (UI.Colour Float)
  }
  deriving (Generic)

instance D.Vars V

deriving via Fields V instance Storable (V I)


newtype IF v = IF
  { colour2 :: v (Colour Float)
  }
  deriving (Generic)

instance D.Vars IF


octree3 :: B ('S2x 'S1) Oct (UI.Colour Float)
octree3 = B $ Oct
  E            (pure red)
  (pure green) E

  (pure blue)  E
  E            (pure white)

octree5 :: B ('S2x ('S2x 'S1)) Oct (UI.Colour Float)
octree5 = B $ Oct
  E       octree3
  octree3 E
  octree3 E
  E       octree3

octree6 :: B ('S2x ('S2x ('S2x 'S1))) Oct (UI.Colour Float)
octree6 = B $ Oct
  E       octree5
  octree5 E
  octree5 E
  E       octree5
