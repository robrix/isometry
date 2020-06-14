{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Isometry.Draw
( runFrame
, frame
) where

import           Control.Carrier.Empty.Church
import           Control.Carrier.Reader
import           Control.Carrier.State.Church
import           Control.Effect.Finally
import           Control.Effect.Lens (use, (%=), (?=))
import           Control.Effect.Lift
import           Control.Effect.Profile
import           Control.Effect.Trace
import           Control.Lens (Lens', (&), (+~), (^.))
import           Control.Monad (when)
import           Control.Monad.IO.Class.Lift
import           Data.Coerce
import           Data.Functor.I
import           Data.Functor.Interval hiding (range)
import           Data.Generics.Product.Fields
import           Data.Ratio
import           Data.Time.Clock
import           Foreign.Storable (Storable)
import           GHC.Generics (Generic)
import           GHC.TypeLits (KnownNat)
import           GL.Array
import           GL.Effect.Check
import           GL.Framebuffer
import           GL.Shader.DSL as D hiding (get, (.*.), (./.), (^.), _x, _xy, _xz, _y, _yz, _z)
import           Graphics.GL.Core41
import qualified Isometry.Draw.Axis as Axis
import           Isometry.Input as Input
import           Isometry.Time
import           Isometry.UI
import           Isometry.View as View
import           Isometry.Voxel (B(..), O(..), Size, sizeO)
import           Linear.V3
import qualified SDL
import qualified UI.Colour as UI
import qualified UI.Drawable as UI
import           UI.Label
import           UI.Typeface
import           UI.Window as Window
import           Unit.Angle
import           Unit.Length
import           Unit.Time

runFrame
  :: ( Has Check sig m
     , Has Finally sig m
     , Has (Lift IO) sig m
     , Has Trace sig m
     )
  => ReaderC Drawable
    (ReaderC (Interval I Int)
    (ReaderC Axis.Drawable
    (StateC UTCTime
    (StateC Player
    (EmptyC
    m))))) a
  -> m ()
runFrame
  = evalEmpty
  . evalState Player{ angle = -pi/4 }
  . (\ m -> now >>= \ start -> evalState start m)
  . Axis.runDrawable
  . runDrawable

newtype Player = Player
  { angle :: I Double
  }
  deriving (Generic)

angle_ :: Lens' Player (I Double)
angle_ = field @"angle"

frame
  :: ( Has Check sig m
     , Has Empty sig m
     , Has (Lift IO) sig m
     , Has Profile sig m
     , Has (Reader Axis.Drawable) sig m
     , Has (Reader Drawable) sig m
     , Has (Reader (Interval I Int)) sig m
     , Has (Reader UI) sig m
     , Has (Reader Window.Window) sig m
     , Has (State Input) sig m
     , Has (State Player) sig m
     , Has (State UTCTime) sig m
     )
  => m ()
frame = timed $ do
  measure "input" Input.input

  dt <- ask @(Seconds _)
  input <- get @Input

  when (input^.pressed_ SDL.KeycodeQ) $ angle_ %= \ angle -> wrap radians (angle + (-turnRate .*. dt))
  when (input^.pressed_ SDL.KeycodeE) $ angle_ %= \ angle -> wrap radians (angle +   turnRate .*. dt)

  angle <- use angle_

  withView angle . measure "draw" . runLiftIO $ do
    UI{ target, face } <- ask
    let font = Font face 18
    bind @Framebuffer Nothing

    v@View{} <- ask

    clipTo v

    glClearColor 0 0 0 0
    glClear GL_COLOR_BUFFER_BIT

    Axis.draw

    UI.using getDrawable $ do
      matrix_ ?= tmap realToFrac (transformToWorld v)

      drawArrays Triangles =<< ask

    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

    measure "setLabel" $ setLabel target font "hello"
    measure "drawLabel" $ drawLabel target 10 UI.white Nothing
  where
  turnRate :: (I :/: Seconds) Double
  turnRate = I pi ./. Seconds 1


octree1 :: O 'L 'L 'L ()
octree1 = OL ()

octree2 :: O ('B 'L 'L) ('B 'L 'L) ('B 'L 'L) ()
octree2 = OO
  (OL ()) (OL ())
  (OL ()) (OL ())

  (OL ()) (OL ())
  (OL ()) (OL ())

octree3 :: O ('B 'L 'L) ('B 'L 'L) ('B 'L 'L) ()
octree3 = OO
  OE      (OL ())
  (OL ()) OE

  (OL ()) OE
  OE      (OL ())

octree4 :: O ('B 'L 'L) ('B 'L 'L) ('B 'L 'L) ()
octree4 = OO
  OE (OL ())
  OE OE

  OE OE
  OE OE


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

makeVertices
  :: ( KnownNat (Size x)
     , KnownNat (Size y)
     , KnownNat (Size z)
     )
  => O x y z ()
  -> [V3 (Metres Float)]
makeVertices o = go 0 d0 o
  where
  d0 = sizeO o
  go
    :: V3 Integer
    -> V3 Integer
    -> O x y z ()
    -> [V3 (Metres Float)]
  go n d@(V3 dx dy dz) = \case
    OE -> []
    OL _ ->  map ((coord <$> n <*> d0) +) vertices
    OX x1 x2 -> go n d' x1 <> go (n + d') d' x2
    OY y1 y2 -> go n d' y1 <> go (n + d') d' y2
    OZ z1 z2 -> go n d' z1 <> go (n + d') d' z2
    OXY x1y1 x2y1
        x1y2 x2y2 -> go n d' x1y1 <> go (n & _x +~ d'^._x) d' x2y1
                  <> go (n & _y +~ d'^._y) d' x1y2 <> go (n & _xy +~ d'^._xy) d' x2y2
    OXZ x1z1 x2z1
        x1z2 x2z2 -> go n d' x1z1 <> go (n & _x +~ d'^._x) d' x2z1
                  <> go (n & _z +~ d'^._z) d' x1z2 <> go (n & _xz +~ d'^._xz) d' x2z2
    OYZ y1z1 y2z1
        y1z2 y2z2 -> go n d' y1z1 <> go (n & _y +~ d'^._y) d' y2z1
                  <> go (n & _z +~ d'^._z) d' y1z2 <> go (n & _yz +~ d'^._yz) d' y2z2
    OO x1y1z1 x2y1z1
       x1y2z1 x2y2z1
       x1y1z2 x2y1z2
       x1y2z2 x2y2z2 -> go n d' x1y1z1 <> go (n & _x +~ d'^._x) d' x2y1z1
                     <> go (n & _y +~ d'^._y) d' x1y2z1 <> go (n & _xy +~ d'^._xy) d' x2y2z1
                     <> go (n & _z +~ d'^._z) d' x1y1z2 <> go (n & _xz +~ d'^._xz) d' x2y1z2
                     <> go (n & _yz +~ d'^._yz) d' x1y2z2 <> go (n + d') d' x2y2z2
      where
      d' = (`div` 2) <$> V3 dx dy dz
    where
    d' = (`div` 2) <$> d
  coord n d = fromRational (succ n % d * toRational d * 2)


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
