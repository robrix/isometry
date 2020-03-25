{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
module GL.Shader.GLSL.Syntax
( Ref
, Expr(..)
, Stmt(..)
) where

newtype Ref a = Ref { getRef :: String }

class Expr expr where
  bool :: Bool -> expr Bool

class Stmt expr ty stmt | stmt -> expr ty where
  var :: ty -> stmt (Ref a)

  if_ :: expr Bool -> stmt () -> stmt () -> stmt ()
