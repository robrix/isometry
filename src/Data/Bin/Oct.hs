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
, bln_
, brn_
, tln_
, trn_
, blf_
, brf_
, tlf_
, trf_
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
    indices = head (deinterleaveWith V2 (deinterleaveWith V2 (deinterleaveWith V2 (V3 <$> [I0, I1] <*> [I0, I1] <*> [I0, I1]))))

instance UnfoldableWithIndex (V3 Bit) Oct where
  iunfoldA f = oct
    <$> f (V3 I0 I0 I0)
    <*> f (V3 I1 I0 I0)
    <*> f (V3 I0 I1 I0)
    <*> f (V3 I1 I1 I0)
    <*> f (V3 I0 I0 I1)
    <*> f (V3 I1 I0 I1)
    <*> f (V3 I0 I1 I1)
    <*> f (V3 I1 I1 I1)

instance Indexed (V3 Bit) Oct where
  o ! i = case i of
    V3 I0 I0 I0 -> o^.bln_
    V3 I1 I0 I0 -> o^.brn_
    V3 I0 I1 I0 -> o^.tln_
    V3 I1 I1 I0 -> o^.trn_
    V3 I0 I0 I1 -> o^.blf_
    V3 I1 I0 I1 -> o^.brf_
    V3 I0 I1 I1 -> o^.tlf_
    V3 I1 I1 I1 -> o^.trf_

instance BinaryIndexed V3 Oct where
  indices = Oct (head (deinterleaveWith V2 (deinterleaveWith V2 (deinterleaveWith V2 (liftA3 V3 [I0, I1] [I0, I1] [I0, I1])))))

instance MutableIndexed (V3 Bit) Oct where
  insert (V3 I0 I0 I0) = set bln_
  insert (V3 I1 I0 I0) = set brn_
  insert (V3 I0 I1 I0) = set tln_
  insert (V3 I1 I1 I0) = set trn_
  insert (V3 I0 I0 I1) = set blf_
  insert (V3 I1 I0 I1) = set brf_
  insert (V3 I0 I1 I1) = set tlf_
  insert (V3 I1 I1 I1) = set trf_

instance Linear.Finite Oct where
  type Size Oct = 8

  fromV (Linear.V v) = Oct (head (deinterleaveWith V2 (deinterleaveWith V2 (deinterleaveWith V2 (map (v V.!) [0..7])))))

oct :: a -> a -> a -> a -> a -> a -> a -> a -> Oct a
oct bln brn tln trn blf brf tlf trf = Oct $ V2 (V2 (V2 bln brn) (V2 tln trn)) (V2 (V2 blf brf) (V2 tlf trf))

oct_ :: Iso' (Oct a) (V2 (V2 (V2 a)))
oct_ = iso getOct Oct

bln_ :: Lens' (Oct a) a
bln_ = oct_._x._x._x

brn_ :: Lens' (Oct a) a
brn_ = oct_._x._x._y

tln_ :: Lens' (Oct a) a
tln_ = oct_._x._y._x

trn_ :: Lens' (Oct a) a
trn_ = oct_._x._y._y

blf_ :: Lens' (Oct a) a
blf_ = oct_._x._x._y

brf_ :: Lens' (Oct a) a
brf_ = oct_._y._x._y

tlf_ :: Lens' (Oct a) a
tlf_ = oct_._x._y._y

trf_ :: Lens' (Oct a) a
trf_ = oct_._y._y._y


type Octree = B Oct
