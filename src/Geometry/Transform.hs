{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Geometry.Transform
( Transform(..)
, mkTranslation
, mkScale
, mkRotation
, apply
, tmap
, (>>>)
) where

import           Control.Category
import           Control.Lens ((&), (.~))
import           Data.Coerce
import           Data.Functor.I
import           Data.Kind (Type)
import           Foreign.Storable
import qualified GL.Type as GL
import           GL.Uniform
import           Linear.Exts
import           Prelude hiding ((.))
import           Unit
import           Unit.Algebra

newtype Transform c (a :: Type -> Type) (b :: Type -> Type) = Transform { getTransform :: M44 c }
  deriving (Show, Storable, GL.Type, Uniform)

instance Num c => Category (Transform c) where
  id = Transform identity
  Transform a . Transform b = Transform (b !*! a)

mkTranslation :: (Num c, Unit d u) => V3 (u c) -> Transform c u u
mkTranslation v = Transform (identity & translation .~ fmap prj v)

mkScale :: forall u v c du dv d' . (Num c, Unit du u, Unit dv v, Unit d' (Div u v)) => V4 (Div u v c) -> Transform c u v
mkScale v = Transform (scaled (prj <$> v))

mkRotation :: Num c => Quaternion (I c) -> Transform c a a
mkRotation q = Transform (identity !*! mkTransformation (coerce q) 0)

apply :: (Num c, Unit d a, Unit d b) => Transform c a b -> V4 (b c) -> V4 (a c)
apply (Transform m) v = pure <$> (m !* fmap prj v)

tmap :: (c -> c') -> Transform c a b -> Transform c' a b
tmap f = Transform . fmap (fmap f) . getTransform
