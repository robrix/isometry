module Geometry.Plane
( signedDistance
) where

import Linear.Metric
import Linear.Vector

signedDistance :: (Metric v, Num a) => v a -> v a -> v a -> a
signedDistance o n p = (p ^-^ o) `dot` n
