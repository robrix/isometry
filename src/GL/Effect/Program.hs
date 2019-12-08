{-# LANGUAGE DeriveFunctor, ExistentialQuantification, ExplicitForAll, LambdaCase, StandaloneDeriving #-}
module GL.Effect.Program
( -- * Program effect
  Program(..)
, build
, use
, set
  -- * Re-exports
, (GL.:::)(..)
, GL.Var(..)
, Algebra
, Has
, run
) where

import Control.Algebra
import qualified GL.Program as GL
import GL.Shader

data Program m k
  = forall ty . Build [(ShaderType, FilePath)] (GL.Program ty -> m k)
  | forall ty . Use (GL.Program ty) (m k)
  | forall name a ty . GL.HasUniform name a ty => Set (GL.Program ty) (GL.Var name a) (m k)

deriving instance Functor m => Functor (Program m)

instance HFunctor Program where
  hmap f = \case
    Build s k -> Build s (f . k)
    Use p   k -> Use p   (f k)
    Set p v k -> Set p v (f k)

instance Effect   Program where
  thread ctx hdl = \case
    Build s k -> Build s (hdl . (<$ ctx) . k)
    Use p   k -> Use p   (hdl (k <$ ctx))
    Set p v k -> Set p v (hdl (k <$ ctx))


build :: forall ty m sig . Has Program sig m => [(ShaderType, FilePath)] -> m (GL.Program ty)
build s = send (Build s pure)

use :: Has Program sig m => (GL.Program ty) -> m ()
use p = send (Use p (pure ()))

set :: (GL.HasUniform name a ty, Has Program sig m) => GL.Program ty -> GL.Var name a -> m ()
set p v = send (Set p v (pure ()))
