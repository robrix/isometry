{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Starlight.Game
( game
) where

import           Control.Algebra
import           Control.Carrier.Finally
import           Control.Carrier.Profile.Tree
import           Control.Carrier.Random.Gen
import           Control.Carrier.Reader
import qualified Control.Carrier.State.STM.TVar as TVar
import           Control.Carrier.State.Church
import           Control.Effect.Lens.Exts as Lens
import           Control.Effect.Trace
import           Control.Monad.Fix
import           Control.Monad.IO.Class.Lift
import           GL
import           GL.Effect.Check
import           Linear.Exts
import qualified SDL
import           Starlight.Draw
import           Starlight.Input
import           Starlight.UI
import           Stochastic.Sample.Markov
import           System.FilePath
import           System.Random.SplitMix (SMGen, newSMGen)
import           UI.Context
import           UI.Label as Label
import           UI.Typeface (cacheCharactersForDrawing, readTypeface)
import qualified UI.Window as Window
import           Unit.Length

type Distance = Metres

runGame
  :: Has (Lift IO) sig m
  => StateC (Chain (V2 (Distance Double)))
    (TVar.StateC Input
    (RandomC SMGen
    (LiftIO
    (FinallyC
    (GLC
    (ReaderC Context
    (ReaderC Window.Window m))))))) a
  -> m a
runGame
  = Window.runSDL
  . Window.runWindow "Starlight" (V2 1024 768)
  . runContext
  . runGLC
  . runFinally
  . runLiftIO
  . (\ m -> sendM newSMGen >>= flip evalRandom m)
  . TVar.evalState @Input mempty
  . evalState (Chain (0 :: V2 (Distance Double)))

game
  :: ( Has Check sig m
     , Has (Lift IO) sig m
     , Has Profile sig m
     , Has Trace sig m
     )
  => m ()
game = runGame $ do
  SDL.cursorVisible SDL.$= False
  trace "loading typeface"
  face <- measure "readTypeface" $ readTypeface ("fonts" </> "DejaVuSans.ttf")
  measure "cacheCharactersForDrawing" . cacheCharactersForDrawing face $ ['0'..'9'] <> ['a'..'z'] <> ['A'..'Z'] <> ",.’/:-⁻⁰¹²³⁴⁵⁶⁷⁸⁹·" -- characters to preload

  target <- measure "label" Label.label

  enabled_ Blend            .= True
  enabled_ DepthClamp       .= True
  enabled_ LineSmooth       .= True
  enabled_ ProgramPointSize .= True
  enabled_ ScissorTest      .= True

  runFrame . runReader UI{ target, face } . fix $ \ loop -> do
    measure "frame" frame
    measure "swap" Window.swap
    loop
