module Isometry.Time
( Instant(..)
, Duration(..)
, now
, since
, timed
) where

import Control.Carrier.Reader
import Control.Effect.Lift
import Control.Effect.State
import Data.Timing
import Unit.Time

timed
  :: ( Has (Lift IO) sig m
     , Has (State Instant) sig m
     )
  => ReaderC (Seconds Double) m a
  -> m a
timed m = do
  dt <- fmap realToFrac . since <$> get <*> now
  put =<< now
  runReader dt m
