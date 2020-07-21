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
import Control.Effect.Time.System as System
import Unit.Time

timed
  :: ( Has (System.Time Instant) sig m
     , Has (State Duration) sig m
     )
  => ReaderC (Seconds Double) m a
  -> m a
timed m = do
  start <- now
  dt <- realToFrac <$> get @Duration
  eraFrom start $ do
    a <- runReader dt m
    put =<< sinceEpoch
    pure a
{-# INLINE timed #-}
