{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
module Isometry.Player
( Player(..)
, angle_
) where

import Control.Lens (Lens', iso)
import Data.Functor.I
import Data.Functor.Interval
import Data.Generics.Product.Fields
import GHC.Generics (Generic)
import Unit.Angle

newtype Player = Player
  { angle :: I Double
  }
  deriving (Generic)

angle_ :: Lens' Player (I Double)
angle_ = field @"angle".iso id (wrap radians)
