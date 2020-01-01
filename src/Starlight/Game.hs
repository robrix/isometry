{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Starlight.Game
( main
) where

import           Control.Carrier.Empty.Maybe
import           Control.Carrier.Finally
import qualified Control.Carrier.Profile.Identity as NoProfile
import qualified Control.Carrier.Profile.Time as Profile
import           Control.Carrier.Reader
import           Control.Carrier.State.Strict
import           Control.Effect.Lens ((%=), (.=))
import qualified Control.Effect.Lens as Lens
import           Control.Effect.Lift
import           Control.Effect.Profile
import qualified Control.Exception.Lift as E
import           Control.Monad (when, (<=<))
import           Control.Monad.IO.Class.Lift (MonadIO)
import           Data.Coerce
import           Data.Function (fix)
import           Data.Functor.Identity
import           Data.Functor.Interval
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Maybe (isJust)
import           Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import           Graphics.GL.Core41
import           Lens.Micro (Lens', each, lens, (^.))
import           Linear.Affine
import           Linear.Exts
import           Linear.Metric
import           Linear.Quaternion
import           Linear.V2 as Linear
import           Linear.V3 as Linear
import           Linear.Vector as Linear
import           Starlight.Actor as Actor
import           Starlight.AI
import           Starlight.Body as Body
import           Starlight.CLI
import           Starlight.Controls
import           Starlight.Draw
import           Starlight.Identifier
import           Starlight.Input
import           Starlight.Physics
import           Starlight.Player
import           Starlight.Radar as Radar
import           Starlight.Ship as Ship
import qualified Starlight.Sol as Sol
import           Starlight.Starfield as Starfield
import           Starlight.System
import           Starlight.View
import           Starlight.Weapon.Laser as Laser
import           System.Environment
import           System.Exit
import           System.FilePath
import           UI.Label as Label
import           UI.Typeface (Font(Font), cacheCharactersForDrawing, readTypeface)
import qualified UI.Window as Window
import           Unit.Length

main :: IO ()
main = handling $ do
  Options{ profile } <- execParser argumentsParser
  if profile then
    Profile.reportTimings . fst <=< Profile.runProfile $ runGame
  else
    NoProfile.runProfile runGame
  where
  handling m = do
    name <- getProgName
    -- Exceptions don’t seem to exit in the repl for unknown reasons, so we catch and log them (except for 'ExitCode')
    if name == "<interactive>" then
      m `E.catches`
        [ E.Handler (const @_ @ExitCode (pure ()))
        , E.Handler (putStrLn . E.displayException @E.SomeException)
        ]
    else
      m

runGame
  :: ( Effect sig
     , Has (Lift IO) sig m
     , Has Profile sig m
     , MonadFail m
     , MonadIO m
     )
  => m ()
runGame = do
  system <- Sol.system

  Window.runWindow "Starlight" (V2 1024 768)
    . runFinally
    . evalState @Input mempty
    . evalState GameState
      { player = Player
        { actor    = Actor
          { position = P (V2 250000 0)
          , velocity = V2 0 150
          , rotation = axisAngle (unit _z) (pi/2)
          , target   = Nothing
          , health   = 1000
          }
        , throttle = 20
        , firing   = False
        }
      , npcs   =
        [ Actor
          { position = P (V2 250000 0)
          , velocity = V2 0 150
          , rotation = axisAngle (unit _z) (pi/2)
          , target   = Just $ Star (10, "Sol")
          , health   = 100
          }
        , Actor
          { position = P (V2 250000 0)
          , velocity = V2 0 150
          , rotation = axisAngle (unit _z) (pi/2)
          , target   = Just $ Star (10, "Sol") :/ (199, "Mercury")
          , health   = 100
          }
        ]
      , system
      } $ do
      face <- measure "readTypeface" $ readTypeface ("fonts" </> "DejaVuSans.ttf")
      measure "cacheCharactersForDrawing" . cacheCharactersForDrawing face $ ['0'..'9'] <> ['a'..'z'] <> ['A'..'Z'] <> "./:-" -- characters to preload

      fpsL    <- measure "label" Label.label
      targetL <- measure "label" Label.label

      glEnable GL_BLEND
      glEnable GL_DEPTH_CLAMP
      glEnable GL_SCISSOR_TEST
      glEnable GL_PROGRAM_POINT_SIZE

      start <- now
      evalState start . runStarfield . runShip . runRadar . runLaser . runBody . fix $ \ loop -> do
        continue <- measure "frame" $ do
          t <- realToFrac <$> since start
          system <- Lens.use _system
          continue <- evalEmpty . runReader (systemAt system (getDelta t)) $ do
            measure "input" input
            dt <- fmap realToFrac . since =<< get
            put =<< now
            measure "controls" $ _player %%= controls dt
            system <- ask
            measure "ai"      (_npcs   . each %= ai      dt system)
            measure "physics" (_actors . each %= physics dt system)
            gameState <- get
            withView gameState (draw dt fpsL targetL (Font face 18) (player gameState) (npcs gameState))
          continue <$ measure "swap" Window.swap
        when continue loop

(%%=) :: Has (State s) sig m => Lens' s a -> StateC a m () -> m ()
lens %%= action = Lens.use lens >>= (`execState` action) >>= (lens .=)

infix 4 %%=

-- | Compute the zoom factor for the given velocity.
--
-- Higher values correlate to more of the scene being visible.
zoomForSpeed :: V2 Int -> Float -> Float
zoomForSpeed size x = runIdentity go where
  go
    | Identity x < min_ speed = min_ zoom
    | Identity x > max_ speed = max_ zoom
    | otherwise               = fromUnit zoom (coerce easeInOutCubic (toUnit speed (Identity x)))
  zoom = Interval 1 6
  speed = speedAt <$> zoom
  speedAt x = x / 25 * fromIntegral (maximum size)

withView
  :: ( Has (Lift IO) sig m
     , Has (Reader Window.Window) sig m
     )
  => GameState
  -> ReaderC View m a
  -> m a
withView game m = do
  scale <- Window.scale
  size  <- Window.size
  let velocity = game ^. _player . _actor . _velocity
      zoom = zoomForSpeed size (norm velocity)
      focus = game ^. _player . _actor . _position
  runReader View{ scale, size, zoom, focus } m

data GameState = GameState
  { player   :: !Player
  , npcs     :: ![Actor]
  , system   :: !(System Body Float)
  }
  deriving (Show)

_player :: Lens' GameState Player
_player = lens player (\ s p -> s { player = p })

_npcs :: Lens' GameState [Actor]
_npcs = lens npcs (\ s n -> s { npcs = n })

_actors :: Lens' GameState (NonEmpty Actor)
_actors = lens ((:|) . actor . player <*> npcs) (\ s (a:|o) -> s { player = (player s) { actor = a }, npcs = o })

_system :: Lens' GameState (System Body Float)
_system = lens system (\ s p -> s { system = p })


now :: Has (Lift IO) sig m => m UTCTime
now = sendM getCurrentTime

since :: Has (Lift IO) sig m => UTCTime -> m NominalDiffTime
since t = flip diffUTCTime t <$> now


evalEmpty :: Functor m => EmptyC m a -> m Bool
evalEmpty = fmap isJust . runEmpty
