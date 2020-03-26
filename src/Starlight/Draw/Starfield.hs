{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
module Starlight.Draw.Starfield
( draw
, Starlight.Draw.Starfield.run
, Drawable
) where

import           Control.Carrier.Reader
import           Control.Effect.Finally
import           Control.Effect.Lens ((?=))
import           Control.Effect.Lift
import           Control.Effect.Trace
import           Control.Lens (Lens')
import           Data.Coerce (coerce)
import           Data.Functor.I
import           Data.Functor.Interval hiding (range)
import           Data.Generics.Product.Fields
import           Foreign.Storable (Storable)
import           GHC.Generics (Generic)
import           GL.Array
import           GL.Effect.Check
import           GL.Shader.DSL hiding ((!*!), (^*))
import qualified GL.Shader.DSL as D
import           Starlight.View
import qualified UI.Drawable as UI
import qualified UI.Window as Window
import           Unit.Length

draw
  :: ( Has Check sig m
     , Has (Lift IO) sig m
     , Has (Reader Drawable) sig m
     , Has (Reader View) sig m
     )
  => m ()
draw = UI.using getDrawable $ do
  view@View{ zoom, focus } <- ask

  resolution_ ?= (fromIntegral <$> contextSize view)
  focus_      ?= focus
  zoom_       ?= realToFrac (1/zoom)

  drawArrays TriangleStrip range


run
  :: ( Has Check sig m
     , Has Finally sig m
     , Has (Lift IO) sig m
     , Has Trace sig m
     )
  => ReaderC Drawable m a
  -> m a
run = UI.loadingDrawable Drawable shader vertices


newtype Drawable = Drawable { getDrawable :: UI.Drawable U V Frag }


vertices :: [V I]
vertices = coerce @[V2 Float]
  [ V2 (-1) (-1)
  , V2   1  (-1)
  , V2 (-1)   1
  , V2   1    1
  ]

range :: Interval I Int
range = 0...length vertices


-- based on Star Nest by Pablo Roman Andrioli: https://www.shadertoy.com/view/XlfGRj

shader :: Shader shader => shader U V Frag
shader
  =   vertex (\ _ V{ pos } None -> main $
    gl_Position .= ext4 (ext3 pos 0) 1)

  >>> fragment (\ U{ resolution, focus, zoom } None Frag{ fragColour } -> main $ do
    resolution <- let' @_ @_ @_ @(V2 Float) "resolution" (coerce resolution)
    uv <- let' "uv" $ (gl_FragCoord^._xy / resolution^._xy - 0.5) * xy 1 (resolution^._y / resolution^._x)
    dir <- var "dir" $ ext3 (uv D.^* zoom) 1 D.^* 0.5
    focus <- var "focus" $ dext3 (coerce focus) 1
    let wrap x = ((x + pi) `mod'` (pi * 2)) - pi
    nf <- let' "nf" (float (0.01 / norm (get focus)))
    a1 <- let' "a1" (wrap (0.3 + nf))
    cos_a1 <- let' "cos_a1" (cos a1)
    sin_a1 <- let' "sin_a1" (sin a1)
    rot1 <- let' "rot1" $ m2 (V2 (xy cos_a1 sin_a1) (xy (-sin_a1) cos_a1))
    a2 <- let' "a2" (wrap (0.2 + nf))
    cos_a2 <- let' "cos_a2" (cos a2)
    sin_a2 <- let' "sin_a2" (sin a2)
    rot2 <- let' "rot2" $ m2 (V2 (xy cos_a2 sin_a2) (xy (-sin_a2) cos_a2))
    dir^^._xz *!= rot1
    dir^^._xy *!= rot2
    focus^^._xz *!= cast @_ @(M22 Double) rot1
    focus^^._xy *!= cast @_ @(M22 Double) rot2
    focus <- let' "focus2" $ cast @_ @(V3 Float) (get focus `mod'` v3 (pure (tile * 2))) * 10
    v <- var "v" $ v3 0
    r <- var @_ @_ @_ @Int "r" 2
    while (get r `lt` volsteps) $ do
      s <- let' "s" (0.1 + 0.125 * float (get r))
      p <- var "p" $ focus + get dir D.^* s
      p .= abs (v3 (pure tile) - (get p `mod'` v3 (pure (tile * 2))))
      pa <- var "pa" 0
      a <- var "a" 0
      i <- var @_ @_ @_ @Int "i" 0
      while (get i `lt` iterations) $ do
        p .= abs (get p) ^/ dot (get p) (get p) - formuparam
        prev <- let' "prev" (get pa)
        pa .= norm (get p)
        a += abs (get pa - prev)
        i += 1
      a .= get a ** 3
      v += xyz s (s ** 2) (s ** 2) D.^* get a D.^* brightness D.^* (0.5 * distfading ** float (get r))
      r += 1
    mag <- let' "mag" (norm (get v))
    v .= lerp saturation (v3 (pure mag)) (get v)
    fragColour .= ext4 (get v D.^* 0.01) 1)
  where
  iterations :: Num a => a
  iterations = 17
  formuparam :: Fractional a => a
  formuparam = 0.53
  volsteps :: Num a => a
  volsteps = 8
  tile :: Fractional a => a
  tile = 1/1.61803398875
  brightness :: Fractional a => a
  brightness = 0.0015
  distfading :: Fractional a => a
  distfading = 0.65
  saturation :: Fractional a => a
  saturation = 0.65


data U v = U
  { resolution :: v (V2 (Window.Coords Float))
  , focus      :: v (V2 (Giga Metres Double))
  , zoom       :: v Float
  }
  deriving (Generic)

instance Vars U

resolution_ :: Lens' (U v) (v (V2 (Window.Coords Float)))
resolution_ = field @"resolution"

focus_ :: Lens' (U v) (v (V2 (Giga Metres Double)))
focus_ = field @"focus"

zoom_ :: Lens' (U v) (v Float)
zoom_ = field @"zoom"

newtype V v = V { pos :: v (V2 Float) }
  deriving (Generic)

instance Vars V

deriving via Fields V instance Storable (V I)
