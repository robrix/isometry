module Linear.Exts
( translated
, rotated
, scaled
, reject
, angleTo
) where

import Lens.Micro ((^.))
import Linear.Matrix
import Linear.Metric
import Linear.V2
import Linear.V3
import Linear.Vector
import Unit.Angle

translated :: V2 Float -> M33 Float
translated (V2 tx ty) = V3
  (V3 1 0 tx)
  (V3 0 1 ty)
  (V3 0 0 1)

rotated :: Radians Float -> M33 Float
rotated (Radians theta) = V3
  (V3 cosT (-sinT) 0)
  (V3 sinT cosT    0)
  (V3 0    0       1) where
  cosT = cos theta
  sinT = sin theta


reject :: (Metric v, Fractional a) => v a -> v a -> v a
reject a b = a ^-^ project a b


-- | The angle from the first vector to the second.
angleTo :: V2 Float -> V2 Float -> Radians Float
angleTo v1 v2 = Radians (atan2 (v2 ^. _y) (v2 ^. _x) - atan2 (v1 ^. _y) (v1 ^. _x))
