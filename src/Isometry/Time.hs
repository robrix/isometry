module Isometry.Time
( Instant(..)
, Duration(..)
, now
, since
, timed
) where

import Control.Effect.State
import Control.Effect.Time.System as System

timed
  :: ( Has (System.Time Instant) sig m
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
