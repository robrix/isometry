module Isometry.Language
( Coords(..)
, Time(..)
) where

import Linear.V3
import Unit.Length

class Coords expr where
  coords :: expr (V3 (Metres Double))

class Time expr where
  time :: expr Double
