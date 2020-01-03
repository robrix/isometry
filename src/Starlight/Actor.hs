module Starlight.Actor
( Actor(..)
, target_
, health_
, HasPosition(..)
, HasVelocity(..)
, HasRotation(..)
) where

import Lens.Micro (Lens', lens)
import Linear.Affine
import Linear.Quaternion
import Linear.V2
import Starlight.Identifier

data Actor = Actor
  { position :: !(Point V2 Float)
  , velocity :: !(V2 Float)
  , rotation :: !(Quaternion Float)
  , target   :: !(Maybe Identifier)
  , health   :: !Float
  }
  deriving (Show)

target_ :: Lens' Actor (Maybe Identifier)
target_ = lens target (\ s t -> s { target = t })

health_ :: Lens' Actor Float
health_ = lens health (\ s h -> s { health = h })


class HasPosition t where
  position_ :: Lens' t (Point V2 Float)

instance HasPosition Actor where
  position_ = lens position (\ s v -> s { position = v })


class HasVelocity t where
  velocity_ :: Lens' t (V2 Float)

instance HasVelocity Actor where
  velocity_ = lens velocity (\ s v -> s { velocity = v })


class HasRotation t where
  rotation_ :: Lens' t (Quaternion Float)

instance HasRotation Actor where
  rotation_ = lens rotation (\ s r -> s { rotation = r })
