{-# LANGUAGE DataKinds, TypeOperators #-}
module UI.Graph.Points
( shader
) where

import GL.Shader.DSL

shader :: Prog
  '[ "matrix"     '::: M33 Float
   , "pointSize"  '::: Float
   , "colour"     '::: Colour Float ]
  '[ "pos"        '::: V2 Float ]
  '[ "fragColour" '::: Colour Float ]
shader = V vertex $ F fragment Nil

vertex :: Shader 'Vertex
  '[ "matrix"    '::: M33 Float
   , "pointSize" '::: Float ]
  '[ "pos"       '::: V2 Float ]
  '[]
vertex = uniforms $ \ matrix pointSize -> inputs $ \ pos -> main $ do
  gl_Position .= vec4 (vec3 ((matrix !* vec3 pos 1) ^. _xy) 0) 1
  gl_PointSize .= pointSize

fragment :: Shader 'Fragment
  '[ "colour"     '::: Colour Float ]
  '[]
  '[ "fragColour" '::: Colour Float ]
fragment = uniforms $ \ colour -> outputs $ \ fragColour -> main $ do
  p <- let' "p" (gl_PointCoord - vec2 0.5 0.5)
  iff (norm p `gt` 1)
    discard
    (do
      mag <- let' "mag" (norm p * 2)
      fragColour .= vec4 (colour ^. _xyz) (1 - mag * mag * mag / 2))
