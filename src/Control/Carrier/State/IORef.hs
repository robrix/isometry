{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Carrier.State.IORef
( -- * State carrier
  runState
, evalState
, execState
, StateC(..)
  -- * State effect
, module Control.Effect.State
) where

import Control.Carrier.Reader
import Control.Effect.State
import Control.Monad.IO.Class.Lift
import Data.IORef

runState :: Has (Lift IO) sig m => s -> StateC s m a -> m (s, a)
runState s (StateC m) = do
  ref <- sendM (newIORef s)
  a <- runReader ref m
  s' <- sendM (readIORef ref)
  pure (s', a)

evalState :: Has (Lift IO) sig m => s -> StateC s m a -> m a
evalState s = fmap snd . runState s

execState :: Has (Lift IO) sig m => s -> StateC s m a -> m s
execState s = fmap fst . runState s

newtype StateC s m a = StateC (ReaderC (IORef s) m a)
  deriving (Applicative, Functor, Monad, MonadIO)
