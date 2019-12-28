{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Carrier.Profile.Time
( -- * Profile carrier
  runProfile
, ProfileC(..)
, Timing(..)
, timing
, mean
, Timings(..)
  -- * Profile effect
, module Control.Effect.Profile
) where

import           Control.Algebra
import           Control.Carrier.Lift
import           Control.Carrier.Writer.Strict
import           Control.Effect.Profile
import           Control.Monad.IO.Class
import qualified Data.Map as Map
import           Data.Text (Text)
import           Data.Time.Clock
import           Prelude hiding (sum)

runProfile :: ProfileC m a -> m (Timings, a)
runProfile (ProfileC m) = runWriter m

newtype ProfileC m a = ProfileC (WriterC Timings m a)
  deriving (Applicative, Functor, Monad, MonadIO)

instance (Has (Lift IO) sig m, Effect sig) => Algebra (Profile :+: sig) (ProfileC m) where
  alg = \case
    L (Measure l m k) -> do
      start <- sendM getCurrentTime
      a <- m
      end <- sendM getCurrentTime
      ProfileC (tell (timing l (end `diffUTCTime` start)))
      k a
    R other -> ProfileC (send (handleCoercible other))


data Timing = Timing
  { sum   :: {-# UNPACK #-} !NominalDiffTime
  , min'  :: {-# UNPACK #-} !NominalDiffTime
  , max'  :: {-# UNPACK #-} !NominalDiffTime
  , count :: {-# UNPACK #-} !Int
  }

instance Semigroup Timing where
  Timing s1 mn1 mx1 c1 <> Timing s2 mn2 mx2 c2 = Timing (s1 + s2) (mn1 `min` mn2) (mx1 `max` mx2) (c1 + c2)

instance Monoid Timing where
  mempty = Timing 0 0 0 0

timing :: Text -> NominalDiffTime -> Timings
timing l t = Timings (Map.singleton l (Timing t t t 1))

mean :: Timing -> NominalDiffTime
mean Timing{ sum, count } = sum / fromIntegral count


newtype Timings = Timings (Map.Map Text Timing)

instance Semigroup Timings where
  Timings t1 <> Timings t2 = Timings (Map.unionWith (<>) t1 t2)

instance Monoid Timings where
  mempty = Timings mempty
