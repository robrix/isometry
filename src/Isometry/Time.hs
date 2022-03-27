module Isometry.Time
( Time(..)
, Instant(..)
, Duration(..)
, Epoch(..)
, now
, since
, timed
, sinceEpoch
) where

import Control.Carrier.Time.System as System
import Control.Effect.Reader
import Control.Effect.State
import Data.Fixed
import Data.Time.Clock.System

newtype Epoch = Epoch { getEpoch :: System.Instant }

timed
  :: ( Has (System.Time Instant) sig m
     , Has (Reader Epoch) sig m
     , Has (State Duration) sig m
     )
  => m a
  -> m a
timed m = do
  start <- now
  eraFrom start $ do
    a <- m
    put =<< sinceEpoch
    pure a
{-# INLINE timed #-}

sinceEpoch
  :: ( Has (Reader Epoch) sig m
     , Has (System.Time Instant) sig m
     )
  => m System.Duration
sinceEpoch = do
  now <- now
  epoch <- asks getEpoch
  let d = sinceInstant epoch now
  d `seq` pure d
{-# INLINE sinceEpoch #-}

eraFrom :: Has (Reader Epoch) sig m => Instant -> m a -> m a
eraFrom = local . const . Epoch

sinceInstant :: Instant -> Instant -> Duration
sinceInstant (Instant (MkSystemTime bs bns)) (Instant (MkSystemTime as ans)) = Duration (realToFrac (as - bs) + MkFixed (fromIntegral ans - fromIntegral bns))
{-# INLINABLE sinceInstant #-}
