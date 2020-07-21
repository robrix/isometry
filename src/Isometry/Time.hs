{-# LANGUAGE TypeApplications #-}
module Isometry.Time
( Instant(..)
, Duration(..)
, now
, since
, timed
) where

import Control.Carrier.Reader
import Control.Effect.State
import Control.Carrier.Time.System as System
import Unit.Time

timed
  :: ( Has (System.Time Instant) sig m
     , Has (State Instant) sig m
     )
  => ReaderC (Seconds Double) m a
  -> m a
timed m = do
  dt <- fmap realToFrac . since <$> get <*> now @Instant
  put =<< now @Instant
  runReader dt m
{-# INLINE timed #-}
