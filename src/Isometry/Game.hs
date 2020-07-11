{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Isometry.Game
( game
) where

import           Control.Algebra
import           Control.Carrier.Finally
import           Control.Carrier.Profile.Tree
import           Control.Carrier.Random.Gen
import           Control.Carrier.Reader
import           Control.Carrier.State.Church
import qualified Control.Carrier.State.STM.TVar as TVar
import           Control.Effect.Lens.Exts as Lens
import           Control.Effect.Thread
import           Control.Effect.Trace
import           Control.Exception.Lift
import           Control.Lens ((^.))
import           Control.Monad (unless, when)
import           Control.Monad.Fix
import           Control.Monad.IO.Class.Lift
import           Data.Functor.Interval hiding (lerp)
import           GL
import           GL.Effect.Check
import           Isometry.Frame
import           Isometry.Input
import           Isometry.Player
import           Isometry.Time
import           Isometry.UI
import           Linear.Exts
import qualified SDL
import           Stochastic.Sample.Markov
import           System.FilePath
import           System.Random.SplitMix (SMGen, newSMGen)
import           UI.Context
import           UI.Label as Label
import           UI.Typeface (cacheCharactersForDrawing, readTypeface)
import qualified UI.Window as Window
import           Unit.Angle
import           Unit.Length
import           Unit.Time

type Distance = Metres

runGame
  :: Has (Lift IO) sig m
  => StateC (Chain (V2 (Distance Double)))
    (TVar.StateC Player
    (TVar.StateC Input
    (RandomC SMGen
    (LiftIO
    (FinallyC
    (GLC
    (ReaderC Context
    (ReaderC Window.Window m)))))))) a
  -> m a
runGame
  = Window.runSDL
  . Window.runWindow "Isometry" (V2 1024 768)
  . runContext
  . runGLC
  . runFinally
  . runLiftIO
  . (\ m -> sendM newSMGen >>= flip evalRandom m)
  . TVar.evalState @Input mempty
  . TVar.evalState Player{ angle = -pi/4 }
  . evalState (Chain (0 :: V2 (Distance Double)))

game
  :: ( Has Check sig m
     , Has (Lift IO) sig m
     , Has Profile sig m
     , HasLabelled Thread (Thread id) sig m
     , Has Trace sig m
     )
  => m ()
game = runGame $ do
  SDL.cursorVisible SDL.$= False
  trace "loading typeface"
  face <- measure "readTypeface" $ readTypeface ("fonts" </> "DejaVuSans.ttf")
  measure "cacheCharactersForDrawing" . cacheCharactersForDrawing face $ ['0'..'9'] <> ['a'..'z'] <> ['A'..'Z'] <> ",.’/:-⁻⁰¹²³⁴⁵⁶⁷⁸⁹·" -- characters to preload

  target <- measure "label" Label.label

  start <- now
  integration <- fork . (>>= throwIO) . evalState start . fix $ \ loop -> do
    err <- try @SomeException . timed $ do
      dt <- ask @(Seconds _)
      input <- get @Input

      let turningL = input^.pressed_ SDL.KeycodeQ
          turningR = input^.pressed_ SDL.KeycodeE
      when turningL $ angle_ += (-turnRate .*. dt)
      when turningR $ angle_ +=   turnRate .*. dt

      current <- use angle_
      let nearest = fromIntegral @Int (round (current / pi * 4)) / 4 * pi
          delta = abs (wrap radians (current - nearest))

      unless (turningL || turningR || delta == 0) $
        -- animate angle to nearest π/4 radians increment
        angle_ %= lerp (getI (min 1 ((turnRate .*. dt) / delta))) nearest

    case err of
      Left err -> pure err
      Right () -> yield >> loop

  enabled_ Blend            .= True
  enabled_ CullFace         .= True
  enabled_ DepthTest        .= True
  enabled_ LineSmooth       .= True
  enabled_ ProgramPointSize .= True
  enabled_ ScissorTest      .= True

  (runFrame . runReader UI{ target, face } . fix $ \ loop -> do
    measure "frame" frame
    measure "swap" Window.swap
    loop)
    `finally` kill integration
  where
  turnRate :: (I :/: Seconds) Double
  turnRate = I pi ./. Seconds 1.5
