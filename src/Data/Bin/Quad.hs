{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Data.Bin.Quad
( Quad(..)
, quad
, quad_
, lb_
, rb_
, lt_
, rt_
, Quadtree
) where

import Control.Applicative (liftA2)
import Control.Lens (Iso', Lens', iso, set, (^.))
import Control.Lens.Indexed hiding (Indexed)
import Data.Bin.Bit
import Data.Bin.Tree
import Data.Functor.C
import Data.Vector as V ((!))
import GHC.Generics (Generic, Generic1)
import Linear.V as Linear
import Linear.V2
import Linear.Vector

-- | Quaternary nodes.
--
-- Mnemonic for fields: bottom/top, left/right.
newtype Quad a = Quad { getQuad :: V2 (V2 a) }
  deriving (Foldable, Functor, Generic, Generic1, Monoid, Semigroup, Traversable)
  deriving (Applicative) via (V2 :.: V2)

instance FoldableWithIndex (V2 Bit) Quad
instance FunctorWithIndex (V2 Bit) Quad
instance TraversableWithIndex (V2 Bit) Quad where
  itraverse f (Quad q) = Quad <$> itraverse (\ ix -> itraverse (\ iy -> f (indices^.el ix.el iy))) q
    where
    indices = head (deinterleaveWith V2 (deinterleaveWith V2 (V2 <$> [B0, B1] <*> [B0, B1])))

instance UnfoldableWithIndex (V2 Bit) Quad where
  iunfoldA f = quad
    <$> f (V2 B0 B0)
    <*> f (V2 B1 B0)
    <*> f (V2 B0 B1)
    <*> f (V2 B1 B1)

instance Indexed (V2 Bit) Quad where
  q ! i = case i of
    V2 B0 B0 -> q^.lb_
    V2 B1 B0 -> q^.rb_
    V2 B0 B1 -> q^.lt_
    V2 B1 B1 -> q^.rt_

instance BinaryIndexed V2 Quad where
  indices = Quad (head (deinterleaveWith V2 (deinterleaveWith V2 (liftA2 V2 [B0, B1] [B0, B1]))))

instance MutableIndexed (V2 Bit) Quad where
  insert (V2 B0 B0) = set lb_
  insert (V2 B1 B0) = set rb_
  insert (V2 B0 B1) = set lt_
  insert (V2 B1 B1) = set rt_

instance Linear.Finite Quad where
  type Size Quad = 4

  fromV (Linear.V v) = Quad (head (deinterleaveWith V2 (deinterleaveWith V2 (map (v V.!) [0..3]))))

quad :: a -> a -> a -> a -> Quad a
quad lb rb lt rt = Quad $ V2 (V2 lb rb) (V2 lt rt)

quad_ :: Iso' (Quad a) (V2 (V2 a))
quad_ = iso getQuad Quad

lb_ :: Lens' (Quad a) a
lb_ = quad_._x._x

rb_ :: Lens' (Quad a) a
rb_ = quad_._x._y

lt_ :: Lens' (Quad a) a
lt_ = quad_._y._x

rt_ :: Lens' (Quad a) a
rt_ = quad_._y._y


type Quadtree = B Quad
