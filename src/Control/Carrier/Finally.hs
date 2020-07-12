{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Carrier.Finally
( -- * Finally carrier
  runFinally
, FinallyC(..)
  -- * Finally effect
, module Control.Effect.Finally
) where

import           Control.Algebra
import           Control.Carrier.State.IORef
import           Control.Effect.Finally
import           Control.Effect.Lift
import qualified Control.Exception.Lift as E
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Data.Foldable (traverse_)
import           Data.Functor (void)
import           Data.IORef

runFinally :: Has (Lift IO) sig m => FinallyC m a -> m a
runFinally (FinallyC m) = do
  ref <- sendM (newIORef [])
  runStateRef ref m `E.finally` (sendM (readIORef ref) >>= traverse_ runFinally)

newtype FinallyC m a = FinallyC { runFinallyC :: StateC [FinallyC m ()] m a }
  deriving (Applicative, Functor, Monad, MonadFail, MonadFix, MonadIO)

instance Has (Lift IO) sig m => Algebra (Finally :+: sig) (FinallyC m) where
  alg hdl sig ctx = case sig of
    L (OnExit m) -> ctx <$ FinallyC (modify (void (hdl (m <$ ctx)) :))
    R other      -> FinallyC (alg (runFinallyC . hdl) (R other) ctx)
