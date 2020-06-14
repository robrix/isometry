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
import           Data.Coerce
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
import           Isometry.Octree as Octree (B(..), Finite(..), O(..), Size)
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



runDrawable
  :: ( Has Check sig m
     , Has Finally sig m
     , Has (Lift IO) sig m
     , Has Trace sig m
     )
  => ReaderC Drawable (ReaderC (Interval I Int) m) a
  -> m a
runDrawable = runReader (0...length vertices) . UI.loadingDrawable Drawable shader vertices
  where vertices = coerce (makeVertices octree3)

makeVertices :: KnownNat (Size s) => O s () -> [V3 (Metres Float)]
makeVertices o = go 0 d0 o
  where
  d0 = Octree.size o
  go
    :: V3 Integer
    -> Integer
    -> O s ()
    -> [V3 (Metres Float)]
  go n d = \case
    OE -> []
    OL _ ->  map (((fromIntegral <$> n * 2) - fromIntegral d0 / 2) +) vertices
    OO x1y1z1 x2y1z1
       x1y2z1 x2y2z1
       x1y1z2 x2y1z2
       x1y2z2 x2y2z2 -> go n d' x1y1z1 <> go (n & _x +~ d') d' x2y1z1
                     <> go (n & _y +~ d') d' x1y2z1 <> go (n & _xy +~ pure d') d' x2y2z1
                     <> go (n & _z +~ d') d' x1y1z2 <> go (n & _xz +~ pure d') d' x2y1z2
                     <> go (n & _yz +~ pure d') d' x1y2z2 <> go (n + pure d') d' x2y2z2
    where
    d' = d `div` 2


newtype Drawable = Drawable { getDrawable :: UI.Drawable U V Frag }


vertices :: [V3 (Metres Float)]
vertices =
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
  =   vertex (\ U{ matrix } V{ pos } None -> main $
    gl_Position .= matrix D.>* ext4 pos 1)

  >>> fragment (\ _ None Frag{ fragColour } -> main $
    fragColour .= v4 UI.white)


newtype U v = U
  { matrix :: v (Transform V4 Float Metres ClipUnits)
  }
  deriving (Generic)

instance D.Vars U

matrix_ :: Lens' (U v) (v (Transform V4 Float Metres ClipUnits))
matrix_ = field @"matrix"


newtype V v = V { pos :: v (V3 (Metres Float)) }
  deriving (Generic)

instance D.Vars V

deriving via Fields V instance Storable (V I)


octree1 :: O 'L ()
octree1 = OL ()

octree2 :: O ('B 'L) ()
octree2 = OO
  (OL ()) (OL ())
  (OL ()) (OL ())

  (OL ()) (OL ())
  (OL ()) (OL ())

octree3 :: O ('B 'L) ()
octree3 = OO
  OE      (OL ())
  (OL ()) OE

  (OL ()) OE
  OE      (OL ())

octree4 :: O ('B 'L) ()
octree4 = OO
  OE (OL ())
  OE OE

  OE OE
  OE OE
