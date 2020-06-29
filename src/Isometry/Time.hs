{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import Data.Fixed
import System.CPUTime
import Unit.Time

newtype Instant = Instant { getInstant :: Fixed E12 }
  deriving (Eq, Fractional, Num, Ord, Real, Show)

newtype Duration = Duration { getDuration :: Fixed E12 }
  deriving (Eq, Fractional, Num, Ord, Real, Show)

now :: Has (Lift IO) sig m => m Instant
now = Instant . MkFixed <$> sendM getCPUTime

since :: Has (Lift IO) sig m => Instant -> m Duration
since (Instant t) = Duration . (t -) . getInstant <$> now

timed
  :: ( Has (Lift IO) sig m
     , Has (State Instant) sig m
     )
  => ReaderC (Seconds Double) m a
  -> m a
timed m = do
  dt <- fmap realToFrac . since =<< get
  put =<< now
  runReader dt m
