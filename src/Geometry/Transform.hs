{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Geometry.Transform
( Transform(..)
, mkTranslation
, mkScale
, mkRotation
, inverse
, apply
, tmap
, identity
, (>>>)
, (<<<)
, (<*<)
, (>*>)
, (>*)
) where

import           Control.Category
import           Control.Lens ((&), (.~))
import           Data.Coerce
import           Data.Functor.I
import           Data.Functor.Rep
import           Data.Kind (Type)
import           Foreign.Storable
import qualified GL.Type as GL
import           GL.Uniform
import           Linear.Exts hiding (identity)
import qualified Linear.Exts as L
import           Prelude hiding ((.))
import           Unit
import           Unit.Algebra

newtype Transform m c (a :: Type -> Type) (b :: Type -> Type) = Transform { getTransform :: m (m c) }

deriving instance Show (m (m c)) => Show (Transform m c a b)
deriving instance Storable (m (m c)) => Storable (Transform m c a b)
deriving instance GL.Type (m (m c)) => GL.Type (Transform m c a b)
deriving instance Uniform (m (m c)) => Uniform (Transform m c a b)

instance (Num c, Additive m, Applicative m, Traversable m) => Category (Transform m c) where
  id = identity
  Transform a . Transform b = Transform (a !*! b)

identity :: (Applicative t, Traversable t, Num c) => Transform t c u u
identity = Transform L.identity

mkTranslation :: (Num c, Unit d u, Applicative m, R4 m, Representable m, Traversable m) => V3 (u c) -> Transform m c u u
mkTranslation v = Transform (L.identity & translation .~ fmap prj v)

mkScale :: forall u v c du dv d' . (Num c, Unit du u, Unit dv v, Unit d' (Div u v)) => V3 (Div u v c) -> Transform V4 c v u
mkScale v = Transform (scaled (point (prj <$> v)))

mkRotation :: Num c => Quaternion (I c) -> Transform V4 c a a
mkRotation q = Transform (L.identity !*! mkTransformation (coerce q) 0)

inverse :: forall c a b . Fractional c => Transform V4 c a b -> Transform V4 c b a
inverse = coerce (inv44 :: M44 c -> M44 c)

apply :: (Num c, Unit d a, Unit d b, Additive m, Foldable m) => Transform m c a b -> m (a c) -> m (b c)
apply (Transform m) v = pure <$> (m !* fmap prj v)

tmap :: Functor m => (c -> c') -> Transform m c a b -> Transform m c' a b
tmap f = Transform . fmap (fmap f) . getTransform

(<*<) :: (Num a, Additive m, Applicative m, Traversable m) => Transform m a v w -> Transform m a u v -> Transform m a u w
(<*<) = (<<<)

(>*>) :: (Num a, Additive m, Applicative m, Traversable m) => Transform m a u v -> Transform m a v w -> Transform m a u w
(>*>) = (>>>)

(>*) :: (Num a, Unit d u, Unit d v, Additive m, Foldable m) => Transform m a u v -> m (u a) -> m (v a)
(>*) = apply

infixl 7 <*<, >*>, >*
