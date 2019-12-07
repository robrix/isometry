{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module UI.Carrier.Window
( -- * Window carrier
  WindowC(..)
  -- * Window effect
, module UI.Effect.Window
) where

import Control.Monad.IO.Class
import UI.Effect.Window

newtype WindowC m a = WindowC (m a)
  deriving (Applicative, Functor, Monad, MonadIO)
