{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Data.Bin.Oct
( Oct(..)
, oct
, oct_
, lbn_
, rbn_
, ltn_
, rtn_
, lbf_
, rbf_
, ltf_
, rtf_
, Octree
) where

import Control.Applicative (liftA3)
import Control.Lens (Iso', Lens', iso, set, (^.))
import Control.Lens.Indexed hiding (Indexed)
import Data.Bin.Bit
import Data.Bin.Tree
import Data.Functor.C
import Data.Vector as V ((!))
import GHC.Generics (Generic, Generic1)
import Linear.V as Linear
import Linear.V2
import Linear.V3
import Linear.Vector

-- | Octonary nodes.
--
-- Mnemonic for fields: bottom/top, left/right, near/far.
newtype Oct a = Oct { getOct :: V2 (V2 (V2 a)) }
  deriving (Foldable, Functor, Generic, Generic1, Monoid, Semigroup, Traversable)
  deriving (Applicative) via (V2 :.: V2 :.: V2)

instance FoldableWithIndex (V3 Bit) Oct
instance FunctorWithIndex (V3 Bit) Oct
instance TraversableWithIndex (V3 Bit) Oct where
  itraverse f (Oct o) = Oct <$> itraverse (\ ix -> itraverse (\ iy -> let ixy = el ix.el iy in itraverse (\ iz -> f (indices^.ixy.el iz)))) o
    where
    indices = head (deinterleaveWith V2 (deinterleaveWith V2 (deinterleaveWith V2 (V3 <$> [B0, B1] <*> [B0, B1] <*> [B0, B1]))))

instance UnfoldableWithIndex (V3 Bit) Oct where
  iunfoldA f = oct
    <$> f (V3 B0 B0 B0)
    <*> f (V3 B1 B0 B0)
    <*> f (V3 B0 B1 B0)
    <*> f (V3 B1 B1 B0)
    <*> f (V3 B0 B0 B1)
    <*> f (V3 B1 B0 B1)
    <*> f (V3 B0 B1 B1)
    <*> f (V3 B1 B1 B1)

instance Indexed (V3 Bit) Oct where
  o ! i = case i of
    V3 B0 B0 B0 -> o^.lbn_
    V3 B1 B0 B0 -> o^.rbn_
    V3 B0 B1 B0 -> o^.ltn_
    V3 B1 B1 B0 -> o^.rtn_
    V3 B0 B0 B1 -> o^.lbf_
    V3 B1 B0 B1 -> o^.rbf_
    V3 B0 B1 B1 -> o^.ltf_
    V3 B1 B1 B1 -> o^.rtf_

instance BinaryIndexed V3 Oct where
  indices = Oct (head (deinterleaveWith V2 (deinterleaveWith V2 (deinterleaveWith V2 (liftA3 V3 [B0, B1] [B0, B1] [B0, B1])))))

instance MutableIndexed (V3 Bit) Oct where
  insert (V3 B0 B0 B0) = set lbn_
  insert (V3 B1 B0 B0) = set rbn_
  insert (V3 B0 B1 B0) = set ltn_
  insert (V3 B1 B1 B0) = set rtn_
  insert (V3 B0 B0 B1) = set lbf_
  insert (V3 B1 B0 B1) = set rbf_
  insert (V3 B0 B1 B1) = set ltf_
  insert (V3 B1 B1 B1) = set rtf_

instance Linear.Finite Oct where
  type Size Oct = 8

  fromV (Linear.V v) = Oct (head (deinterleaveWith V2 (deinterleaveWith V2 (deinterleaveWith V2 (map (v V.!) [0..7])))))

oct :: a -> a -> a -> a -> a -> a -> a -> a -> Oct a
oct lbn rbn ltn rtn lbf rbf ltf rtf = Oct $ V2 (V2 (V2 lbn rbn) (V2 ltn rtn)) (V2 (V2 lbf rbf) (V2 ltf rtf))

oct_ :: Iso' (Oct a) (V2 (V2 (V2 a)))
oct_ = iso getOct Oct

lbn_ :: Lens' (Oct a) a
lbn_ = oct_._x._x._x

rbn_ :: Lens' (Oct a) a
rbn_ = oct_._x._x._y

ltn_ :: Lens' (Oct a) a
ltn_ = oct_._x._y._x

rtn_ :: Lens' (Oct a) a
rtn_ = oct_._x._y._y

lbf_ :: Lens' (Oct a) a
lbf_ = oct_._x._x._y

rbf_ :: Lens' (Oct a) a
rbf_ = oct_._y._x._y

ltf_ :: Lens' (Oct a) a
ltf_ = oct_._x._y._y

rtf_ :: Lens' (Oct a) a
rtf_ = oct_._y._y._y


type Octree = B Oct
