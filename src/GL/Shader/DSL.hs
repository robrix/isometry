{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module GL.Shader.DSL
( Shader
, program
, Stage
, vertex
, geometry
, fragment
, (Cat.>>>)
, None(..)
, shaderSources
, Frag(..)
, module GL.Shader.Decl
, module GL.Shader.Expr
, module GL.Shader.Stmt
  -- * Re-exports
, Fields(..)
, Vars
, Colour
, M22
, M33
, M44
, TextureUnit
, V2
, V3
, V4
) where

import qualified Control.Category as Cat
import           Data.Coerce
import           Data.Function (fix)
import           Data.Functor.C
import           Data.Functor.K
import           Data.Kind (Type)
import           Data.Text.Prettyprint.Doc hiding (dot)
import           Data.Text.Prettyprint.Doc.Render.String
import           GHC.Generics hiding ((:.:))
import qualified GL.Shader as Shader
import           GL.Shader.Decl hiding (Type)
import           GL.Shader.Expr
import           GL.Shader.Stmt
import           GL.Shader.Vars
import           GL.TextureUnit
import qualified GL.Uniform as GL
import           Linear.Matrix (M22, M33, M44)
import           Linear.V2 (V2(..))
import           Linear.V3 (V3(..))
import           Linear.V4 (V4(..))
import           Prelude hiding (break)
import           UI.Colour (Colour)

data Shader (u :: (Type -> Type) -> Type) (i :: (Type -> Type) -> Type) (o :: (Type -> Type) -> Type) where
  Shader :: Vars u => ((forall k . u (Expr k)) -> Stage i o) -> Shader u i o

program :: Vars u => ((forall k . u (Expr k)) -> Stage i o) -> Shader u i o
program = Shader


data Stage i o where
  Id :: Stage i i
  (:>>>) :: Stage i x -> Stage x o -> Stage i o
  V :: (Vars i, Vars o) => (i (Expr 'Shader.Vertex)   -> o (Ref 'Shader.Vertex)   -> Decl 'Shader.Vertex   ()) -> Stage i o
  G :: (Vars i, Vars o) => (i (Expr 'Shader.Geometry :.: []) -> o (Ref 'Shader.Geometry) -> Decl 'Shader.Geometry ()) -> Stage i o
  F :: (Vars i, Vars o) => (i (Expr 'Shader.Fragment) -> o (Ref 'Shader.Fragment) -> Decl 'Shader.Fragment ()) -> Stage i o

vertex   :: (Vars i, Vars o) => (i (Expr 'Shader.Vertex)   -> o (Ref 'Shader.Vertex)   -> Decl 'Shader.Vertex   ()) -> Stage i o
vertex = V

geometry :: (Vars i, Vars o) => (i (Expr 'Shader.Geometry :.: []) -> o (Ref 'Shader.Geometry) -> Decl 'Shader.Geometry ()) -> Stage i o
geometry = G

fragment :: (Vars i, Vars o) => (i (Expr 'Shader.Fragment) -> o (Ref 'Shader.Fragment) -> Decl 'Shader.Fragment ()) -> Stage i o
fragment = F

instance Cat.Category Stage where
  id = Id
  (.) = flip (:>>>)


data None (v :: Type -> Type) = None
  deriving (Generic)

instance Vars None

shaderSources :: Shader u i o -> [(Shader.Stage, String)]
shaderSources (Shader f) = fmap (renderString . layoutPretty defaultLayoutOptions) <$> stageSources u' (f u) where
  u = makeVars (Expr . pretty . name)
  u' = foldVars (getK . value) (makeVars (pvar "uniform" . name) `like` u)

stageSources :: Doc () -> Stage i o -> [(Shader.Stage, Doc ())]
stageSources u = \case
  Id  -> []
  V s -> [renderStage Shader.Vertex   id s]
  G s -> [renderStage Shader.Geometry (coerce :: forall x . Expr 'Shader.Geometry x -> (Expr 'Shader.Geometry :.: []) x) s]
  F s -> [renderStage Shader.Fragment id s]
  l :>>> r -> stageSources u l <> stageSources u r
  where
  renderStage :: (Vars i, Vars o) => Shader.Stage -> (forall a . Expr k a -> f a) -> (i f -> o (Ref k) -> Decl k ()) -> (Shader.Stage, Doc ())
  renderStage t g f = (,) t
    $  pretty "#version 410" <> hardline
    <> u
    <> case t of
      Shader.Geometry -> foldVars (getK . value) (makeVars (pvar "in" . (<> "[]") . name) `like` i)
      _               -> foldVars (getK . value) (makeVars (pvar "in"      . name) `like` i)
    <> foldVars (getK . value) (makeVars (pvar "out"     . name) `like` o)
    <> renderDecl (f i o) where
    i = makeVars (g . Expr . pretty . name)
    o = makeVars (Ref . name)

like :: t (K a) -> t b -> t (K a)
like = const

pvar :: GL.Uniform a => String -> String -> K (Doc ()) a
pvar qual n = fix $ \ c -> K $ pretty qual <+> renderTypeOf c <+> pretty n <> pretty ';' <> hardline


newtype Frag v = Frag { fragColour :: v (Colour Float) }
  deriving (Generic)

instance Vars Frag
