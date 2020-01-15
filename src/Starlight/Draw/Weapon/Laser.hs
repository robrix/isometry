{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
module Starlight.Draw.Weapon.Laser
( runLaser
, drawLaser
, Drawable
) where

import           Control.Carrier.Reader
import           Control.Effect.Finally
import           Control.Effect.Lens ((?=))
import           Control.Effect.Lift
import           Control.Effect.Trace
import           Control.Lens (Lens', (^.))
import           Data.Coerce (coerce)
import           Data.Functor.Identity
import           Data.Functor.Interval
import           Data.Generics.Product.Fields
import           Foreign.Storable (Storable)
import           GHC.Generics (Generic)
import           GL.Array
import           GL.Effect.Check
import           GL.Object
import           GL.Shader.DSL hiding (coerce, (!*), (!*!), (^.), _z)
import qualified GL.Shader.DSL as D
import           Starlight.Actor
import           Starlight.View
import qualified Starlight.Weapon.Laser as S
import qualified UI.Drawable as UI
import           Unit.Length

runLaser
  :: ( Has Check sig m
     , Has Finally sig m
     , Has (Lift IO) sig m
     , Has Trace sig m
     , Effect sig
     )
  => ReaderC Drawable m a
  -> m a
runLaser = UI.loadingDrawable Drawable shader vertices


drawLaser
  :: ( Has Check sig m
     , Has (Lift IO) sig m
     , Has (Reader Drawable) sig m
     , Has (Reader View) sig m
     )
  => S.Beam
  -> m ()
drawLaser beam@S.Beam{ colour } = UI.using getDrawable $ do
  view <- ask
  matrix_ ?=
    (   transformToSystem view
    >>> transformToActor (beam^.actor_)
    >>> mkScale (pure 1000))
  colour_ ?= colour

  drawArrays Lines range


newtype Drawable = Drawable { getDrawable :: UI.Drawable U V O }


vertices :: [V Identity]
vertices = coerce @[Float] [0, 1]

range :: Interval Identity Int
range = Interval 0 (Identity (length vertices))


shader :: Shader U V O
shader = program $ \ u
  ->  vertex (\ V{ r } None -> main $
    gl_Position .= D.coerce (matrix u) D.!* vec4 r 0 0 1)
  >>> fragment (\ None O{ fragColour } -> main $
    fragColour .= colour u)


data U v = U
  { matrix :: v (Transform Float ClipUnits (Mega Metres))
  , colour :: v (Colour Float)
  }
  deriving (Generic)

instance Vars U

matrix_ :: Lens' (U v) (v (Transform Float ClipUnits (Mega Metres)))
matrix_ = field @"matrix"

colour_ :: Lens' (U v) (v (Colour Float))
colour_ = field @"colour"


newtype V v = V { r :: v Float }
  deriving (Generic)

instance Vars V

deriving instance Bind     (v Float) => Bind     (V v)
deriving instance Storable (v Float) => Storable (V v)

newtype O v = O { fragColour :: v (Colour Float) }
  deriving (Generic)

instance Vars O
