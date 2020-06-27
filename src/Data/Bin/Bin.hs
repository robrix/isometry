{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Bin.Bin
( Bin(..)
, bin
, bin_
, l_
, r_
, Bintree
) where

import Control.Lens (Iso', Lens', iso, set, (^.))
import Control.Lens.Indexed hiding (Indexed)
import Data.Bin.Bit
import Data.Bin.Tree
import Data.Coerce (coerce)
import Data.Vector as V ((!))
import GHC.Generics (Generic, Generic1)
import Linear.V as Linear
import Linear.V1
import Linear.V2
import Linear.Vector

-- | Binary nodes.
--
-- Mnemonic for fields: left/right.
newtype Bin a = Bin { getBin :: V2 a }
  deriving (Applicative, Foldable, Functor, Generic, Generic1, Monoid, Semigroup, Traversable)

instance FoldableWithIndex (V1 Bit) Bin
instance FunctorWithIndex (V1 Bit) Bin
instance TraversableWithIndex (V1 Bit) Bin where
  itraverse f (Bin b) = Bin <$> itraverse (\ ix -> f (indices^.el ix)) b
    where
    indices = head (deinterleaveWith V2 (V1 <$> [I0, I1]))

instance Indexed (V1 Bit) Bin where
  b ! i = case i of
    V1 I0 -> b^.l_
    V1 I1 -> b^.r_

instance BinaryIndexed V1 Bin where
  indices = Bin (head (deinterleaveWith V2 (map V1 [I0, I1])))

instance MutableIndexed (V1 Bit) Bin where
  insert (V1 I0) = set l_
  insert (V1 I1) = set r_

instance UnfoldableWithIndex (V1 Bit) Bin where
  iunfoldA f = bin
    <$> f (V1 I0)
    <*> f (V1 I1)

instance Linear.Finite Bin where
  type Size Bin = 2

  fromV (Linear.V v) = Bin (head (deinterleaveWith V2 (map (v V.!) [0..1])))

instance R1 Bin where
  _x = _xy._x

instance R2 Bin where
  _xy = bin_

bin :: forall a . a -> a -> Bin a
bin = coerce (V2 :: a -> a -> V2 a)

bin_ :: Iso' (Bin a) (V2 a)
bin_ = iso getBin Bin

l_ :: Lens' (Bin a) a
l_ = bin_._x

r_ :: Lens' (Bin a) a
r_ = bin_._y


type Bintree = B Bin
