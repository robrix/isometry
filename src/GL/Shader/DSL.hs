{-# LANGUAGE AllowAmbiguousTypes, DataKinds, FlexibleInstances, FunctionalDependencies, GADTs, KindSignatures, LambdaCase, ScopedTypeVariables, TypeApplications, TypeOperators, UndecidableInstances #-}
module GL.Shader.DSL
( Shader
, Stmt
, Expr
, Ref
, Prj
, main
, let'
, vec2
, vec3
, vec4
, len
, coerce
, gl_Position
, gl_PointSize
, gl_PointCoord
, discard
, iff
, gt
, (.=)
, (^.)
, _x
, _y
, _z
, _w
, _xy
, _xyz
, _a
, (^*)
, (!*)
, renderShader
, renderExpr
) where

import Control.Monad (ap)
import qualified Data.Coerce as C
import Data.DSL
import Data.Proxy
import Data.Text.Prettyprint.Doc
import GHC.TypeLits
import GL.Shader (Type(..))
import Linear.Matrix (M33)
import Linear.V2 (V2(..))
import Linear.V3 (V3(..))
import Linear.V4 (V4(..))
import UI.Colour (Colour)
import Unit.Angle

data Shader (k :: Type) (u :: Context) (i :: Context) (o :: Context) where
  Uniform :: Shader k u i o -> Shader k (n '::: t ': u) i o
  Input   :: Shader k u i o -> Shader k u (n '::: t ': i) o
  Output  :: Shader k u i o -> Shader k u i (n '::: t ': o)
  Main    :: Stmt k () -> Shader k u i o


data Stmt (k :: Type) a

instance Functor (Stmt k) where
  fmap _ _ = undefined

instance Applicative (Stmt k) where
  pure _ = undefined
  (<*>) = ap

instance Monad (Stmt k) where
  _ >>= _ = undefined


data Expr (k :: Type) a where
  Var :: String -> Expr k a
  Lit :: Double -> Expr k a

  (:+) :: Expr k a -> Expr k a -> Expr k a
  (:*) :: Expr k a -> Expr k a -> Expr k a
  (:-) :: Expr k a -> Expr k a -> Expr k a
  Signum :: Expr k a -> Expr k a
  Negate :: Expr k a -> Expr k a
  Abs :: Expr k a -> Expr k a

  (:/) :: Expr k a -> Expr k a -> Expr k a

  Exp :: Expr k a -> Expr k a
  Log :: Expr k a -> Expr k a
  Sqrt :: Expr k a -> Expr k a
  (:**) :: Expr k a -> Expr k a -> Expr k a
  Sin :: Expr k a -> Expr k a
  Cos :: Expr k a -> Expr k a
  Tan :: Expr k a -> Expr k a
  ASin :: Expr k a -> Expr k a
  ACos :: Expr k a -> Expr k a
  ATan :: Expr k a -> Expr k a
  SinH :: Expr k a -> Expr k a
  CosH :: Expr k a -> Expr k a
  TanH :: Expr k a -> Expr k a
  ASinH :: Expr k a -> Expr k a
  ACosH :: Expr k a -> Expr k a
  ATanH :: Expr k a -> Expr k a

infixl 6 :+
infixl 7 :*
infixl 6 :-
infixl 7 :/
infixr 8 :**

instance Num (Expr k a) where
  (+) = (:+)
  (*) = (:*)
  (-) = (:-)
  signum = Signum
  negate = Negate
  abs = Abs
  fromInteger = Lit . fromInteger

instance Fractional (Expr k a) where
  (/) = (:/)
  fromRational = Lit . fromRational

instance Floating (Expr k a) where
  pi = Lit pi
  exp = Exp
  log = Log
  sqrt = Sqrt
  (**) = (:**)
  sin = Sin
  cos = Cos
  tan = Tan
  asin = ASin
  acos = ACos
  atan = ATan
  sinh = SinH
  cosh = CosH
  tanh = TanH
  asinh = ASinH
  acosh = ACosH
  atanh = ATanH


newtype Ref (k :: Type) t = Ref { getRef :: String }

data Prj s t


main :: Stmt k () -> Shader k u i o
main = Main


let' :: Expr k a -> Stmt k (Expr k a)
let' _ = undefined


vec2 :: Expr k Float -> Expr k Float -> Expr k (V2 Float)
vec2 _ _ = undefined

vec3 :: Expr k (V2 Float) -> Expr k Float -> Expr k (V3 Float)
vec3 _ _ = undefined

vec4 :: Expr k (V3 Float) -> Expr k Float -> Expr k (V4 Float)
vec4 _ _ = undefined

len :: Expr k (v Float) -> Expr k Float
len _ = undefined


coerce :: C.Coercible a b => (a -> b) -> Expr k a -> Expr k b
coerce _ _ = undefined


gl_Position :: Ref 'Vertex (V4 Float)
gl_Position = undefined

gl_PointSize :: Ref 'Vertex Float
gl_PointSize = undefined


gl_PointCoord :: Expr 'Fragment (V2 Float)
gl_PointCoord = undefined

discard :: Stmt 'Fragment ()
discard = undefined


iff :: Expr k Bool -> Stmt k () -> Stmt k () -> Stmt k ()
iff _c _t _e = undefined

gt :: Expr k Float -> Expr k Float -> Expr k Bool
gt _ _ = undefined

infix 4 `gt`


(.=) :: Ref k a -> Expr k a -> Stmt k ()
_ .= _ = undefined

infixr 4 .=

(^.) :: Expr k a -> Prj a b -> Expr k b
_ ^. _ = undefined

infixl 8 ^.

_x :: Prj (v a) a
_x = undefined

_y :: Prj (v a) a
_y = undefined

_z :: Prj (v a) a
_z = undefined

_w :: Prj (v a) a
_w = undefined

_xy :: Prj (v a) (V2 a)
_xy = undefined

_xyz :: Prj (v a) (V3 a)
_xyz = undefined

_a :: Prj (v a) a
_a = undefined


(^*) :: Expr k (v a) -> Expr k a -> Expr k (v a)
_ ^* _ = undefined

infixl 7 ^*


(!*) :: Expr k (M33 Float) -> Expr k (V3 Float) -> Expr k (V3 Float)
_ !* _ = undefined

infixl 7 !*


renderShader :: Shader k u i o -> Doc ()
renderShader _ = undefined

renderExpr :: Expr k a -> Doc ()
renderExpr = go where
  go = parens . \case
    Var n -> pretty n
    Lit d -> pretty d
    a :+ b -> go a <+> pretty '+' <+> go b
    a :* b -> go a <+> pretty '*' <+> go b
    a :- b -> go a <+> pretty '-' <+> go b
    a :/ b -> go a <+> pretty '/' <+> go b
    Signum a -> fn "signum" [go a] -- log
    Negate a -> pretty "-" <> go a
    Abs a -> fn "abs" [go a]
    Exp a -> fn "exp" [go a]
    Log a -> fn "log" [go a]
    Sqrt a -> fn "sqrt" [go a]
    a :** b -> fn "pow" [go a, go b]
    Sin a -> fn "sin" [go a]
    Cos a -> fn "cos" [go a]
    Tan a -> fn "tan" [go a]
    ASin a -> fn "asin" [go a]
    ACos a -> fn "acos" [go a]
    ATan a -> fn "atan" [go a]
    SinH a -> fn "sinh" [go a]
    CosH a -> fn "cosh" [go a]
    TanH a -> fn "tanh" [go a]
    ASinH a -> fn "asinh" [go a]
    ACosH a -> fn "acosh" [go a]
    ATanH a -> fn "atanh" [go a]
  fn n as = pretty n <> tupled as


_radarVertex
  :: Shader
    'Vertex
    '[ "matrix" '::: M33 Float
     , "angle"  '::: Radians Float
     , "sweep"  '::: Radians Float
     ]
    '[ "n" '::: Float ]
    '[]
_radarVertex = mk $ \ matrix angle sweep n -> do
  angle <- let' (coerce getRadians angle + n * coerce getRadians sweep)
  pos <- let' (vec2 (cos angle) (sin angle) ^* 150)
  gl_Position .= vec4 (vec3 ((matrix !* vec3 pos 1) ^. _xy) 0) 1

_pointsVertex
  :: Shader
    'Vertex
    '[ "matrix" '::: M33 Float
     , "pointSize" '::: Float
     ]
    '[ "pos" '::: V2 Float ]
    '[]
_pointsVertex = mk $ \ matrix pointSize pos -> do
  gl_Position .= vec4 (vec3 ((matrix !* vec3 pos 1) ^. _xy) 0) 1
  gl_PointSize .= pointSize

_pointsFragment
  :: Shader
    'Fragment
    '[ "colour"     '::: Colour Float ]
    '[]
    '[ "fragColour" '::: Colour Float ]
_pointsFragment = mk $ \ colour fragColour -> do
  p <- let' (gl_PointCoord - vec2 0.5 0.5)
  iff (len p `gt` 1)
    discard
    (do
      mag <- let' (len p * 2)
      fragColour .= vec4 (colour ^. _xyz) (1 - mag * mag * mag / 2))

_shipVertex
  :: Shader
    'Vertex
    '[ "matrix" '::: M33 Float ]
    '[ "position2" '::: V2 Float ]
    '[]
_shipVertex = mk $ \ matrix pos ->
  gl_Position .= vec4 (matrix !* vec3 pos 1) 1

_shipFragment
  :: Shader
    'Fragment
    '[ "colour"     '::: Colour Float ]
    '[]
    '[ "fragColour" '::: Colour Float ]
_shipFragment = mk $ \ colour fragColour ->
  fragColour .= colour


class Mk k u i o a | k u i o -> a where
  mk :: a -> Shader k u i o

instance Mk k '[] '[] '[] (Stmt k ()) where
  mk = Main

instance {-# OVERLAPPABLE #-} (KnownSymbol n, Mk k '[] '[] os a) => Mk k '[] '[] (n '::: t ': os) (Ref k t -> a) where
  mk f = Output (mk (f (Ref (symbolVal (Proxy @n)))))

instance {-# OVERLAPPABLE #-} (KnownSymbol n, Mk k '[] is os a) => Mk k '[] (n '::: t ': is) os (Expr k t -> a) where
  mk f = Input (mk (f (Var (symbolVal (Proxy @n)))))

instance {-# OVERLAPPABLE #-} (KnownSymbol n, Mk k us is os a) => Mk k (n '::: t ': us) is os (Expr k t -> a) where
  mk f = Uniform (mk (f (Var (symbolVal (Proxy @n)))))
