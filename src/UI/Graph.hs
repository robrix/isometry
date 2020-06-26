{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module UI.Graph
( Graph
, mkGraph
, drawGraph
) where

import           Control.Carrier.Finally
import           Control.Effect.Lens ((?=))
import           Control.Effect.Trace (Trace)
import           Control.Lens ((^.))
import           Control.Monad.IO.Class.Lift
import           Data.Coerce
import           Data.Functor.I
import           Data.Functor.Interval
import           GL.Array
import           GL.Effect.Check
import           GL.Program
import           GL.Shader.DSL (ClipUnits(..))
import           Graphics.GL.Core41
import           Linear.Exts
import           UI.Colour
import qualified UI.Graph.Lines as Lines
import qualified UI.Graph.Points as Points
import           UI.Graph.Vertex

data Graph = Graph
  { matrix    :: !(M33 (ClipUnits Float))
  , colour    :: !(Colour Float)
  , array     :: !(Array (V I))
  , points    :: !(Program Points.U V Points.Frag)
  , lines     :: !(Program Lines.U  V Lines.Frag)
  , pointSize :: !Float
  , count     :: !Int
  }

mkGraph :: (Has Check sig m, Has Finally sig m, Has (Lift IO) sig m, Has Trace sig m) => (Float -> Float) -> Int -> Float -> Float -> m Graph
mkGraph f n from to = do
  let vertex = V2 <*> f
      count = max n 0 + 2
      vertices = map (\ i -> vertex (from + (to - from) * fromIntegral i / fromIntegral (count - 1))) [0..n+1]
      minXY = V2 from (minimum (map (^. _y) vertices))
      maxXY = V2 to   (maximum (map (^. _y) vertices))
      matrix
        =   coerce
        $   translated (-1)
        !*! scaled     (ext (2 / (maxXY - minXY)) 1)
        !*! translated (negated minXY)
      colour = white
  (_, array) <- load (coerce @[V2 Float] vertices)
  points <- build Points.shader
  lines <- build Lines.shader

  pure $! Graph { matrix, colour, array, points, lines, pointSize = 9, count }

drawGraph :: (Has Check sig m, Has (Lift IO) sig m) => Graph -> m ()
drawGraph Graph { matrix, colour, array, points, lines, pointSize, count } = bindArray array $ do
  runLiftIO (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
  use points $ do
    Points.matrix_    ?= matrix
    Points.pointSize_ ?= pointSize
    Points.colour_    ?= colour
    drawArrays Points    (0...count)
  use lines $ do
    Lines.matrix_ ?= matrix
    Lines.colour_ ?= colour
    drawArrays LineStrip (0...count)
