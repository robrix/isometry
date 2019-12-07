{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, LambdaCase, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Carrier.State.IORef
( -- * State carrier
  runStateRef
, runState
, evalState
, execState
, StateC(..)
  -- * State effect
, module Control.Effect.State
) where

import Control.Algebra
import Control.Carrier.Reader
import Control.Effect.State
import Control.Monad.IO.Class.Lift
import Data.IORef

runStateRef :: Has (Lift IO) sig m => s -> StateC s m a -> m (IORef s, a)
runStateRef s (StateC m) = do
  ref <- sendM (newIORef s)
  (,) ref <$> runReader ref m

runState :: Has (Lift IO) sig m => s -> StateC s m a -> m (s, a)
runState s m = do
  (ref, a) <- runStateRef s m
  s' <- sendM (readIORef ref)
  pure (s', a)

evalState :: Has (Lift IO) sig m => s -> StateC s m a -> m a
evalState s = fmap snd . runState s

execState :: Has (Lift IO) sig m => s -> StateC s m a -> m s
execState s = fmap fst . runState s

newtype StateC s m a = StateC (ReaderC (IORef s) m a)
  deriving (Applicative, Functor, Monad, MonadIO)

instance Has (Lift IO) sig m => Algebra (State s :+: sig) (StateC s m) where
  alg = \case
    L (Get   k) -> StateC ask >>= sendM . readIORef         >>= k
    L (Put s k) -> StateC ask >>= sendM . flip writeIORef s >>  k
    R other     -> StateC (send (handleCoercible other))
