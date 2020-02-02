{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
-- | An 'Actor' has 'position', 'velocity', and 'rotation', and can be acted on by the physics simulation.
module Starlight.Actor
( Actor(..)
, transformToActor
, applyImpulse
, HasActor(..)
) where

import Control.Effect.Lens.Exts (asserting)
import Control.Lens (Lens', lens, mapping, none, (&), (+~), (.~), (^.))
import Data.Generics.Product.Fields
import Geometry.Transform
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Linear.Quaternion
import Linear.V3
import Starlight.Physics
import Unit.Algebra
import Unit.Force
import Unit.Length
import Unit.Mass
import Unit.Time

data Actor = Actor
  { position  :: !(V3 (Distance Double))
  , velocity  :: !(V3 ((Distance :/: Seconds) Double))
  , rotation  :: !(Quaternion (I Double))
  , mass      :: !(Kilo Grams Double)
  , magnitude :: !(Distance Double) -- approx. equivalent to diameter; should bound the actor’s geometry
  }
  deriving (Generic, Show)

transformToActor :: Actor -> Transform Double Distance Distance
transformToActor Actor{ position, rotation } = mkTranslation position >>> mkRotation rotation

applyImpulse :: HasCallStack => V3 (Newtons Double) -> Seconds Double -> Actor -> Actor
applyImpulse force dt a = a & momentum_ +~ force ^*. dt


class HasActor t where
  actor_ :: Lens' t Actor

  position_ :: HasCallStack => Lens' t (V3 (Distance Double))
  position_ = actor_.field @"position".asserting (none isNaN)

  velocity_ :: HasCallStack => Lens' t (V3 ((Distance :/: Seconds) Double))
  velocity_ = actor_.field @"velocity".asserting (none isNaN)

  rotation_ :: HasCallStack => Lens' t (Quaternion (I Double))
  rotation_ = actor_.field @"rotation".asserting (none isNaN)

  mass_ :: HasCallStack => Lens' t (Kilo Grams Double)
  mass_ = actor_.field @"mass".asserting (not.isNaN)

  momentum_ :: HasCallStack => Lens' t (V3 ((Kilo Grams :*: Metres :/: Seconds) Double))
  momentum_ = lens get set where
    get :: HasActor t => t -> V3 ((Kilo Grams :*: Metres :/: Seconds) Double)
    get t = (t^.mass_ .*^ t^.velocity_)^.mapping converting
    set :: HasActor t => t -> V3 ((Kilo Grams :*: Metres :/: Seconds) Double) -> t
    set t p = t & velocity_.mapping converting .~ p ^/. t^.mass_

  magnitude_ :: HasCallStack => Lens' t (Distance Double)
  magnitude_ = actor_.field @"magnitude".asserting (not.isNaN)

  projected :: HasCallStack => Seconds Double -> t -> V3 (Distance Double)
  projected dt a = a^.position_ + a^.velocity_ ^*. dt

instance HasActor Actor where
  actor_ = id
