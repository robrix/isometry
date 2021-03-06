{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Unit
( -- * Units
  Unit(..)
  -- ** Conversion
, convert
, converting
  -- ** Comparison
, (.==.)
, compareU
, (.<.)
, (.>.)
, (.<=.)
, (.>=.)
  -- ** Formatting
, formatWith
, format
, formatDec
, formatExp
, formatExpR
, superscript
, Formatting(..)
) where

import Control.Lens.Iso
import Data.Char
import Data.Coerce
import Data.Foldable (foldl')
import Data.Functor.I
import Data.Functor.Identity
import Data.Functor.K
import Data.Kind (Type)
import Numeric

-- * Units

class ( Applicative u
      , forall a b . Coercible a b => Coercible (u a) (u b)
      , forall a . Eq a => Eq (u a)
      , forall a . Floating a => Floating (u a)
      , forall a . Fractional a => Fractional (u a)
      , forall a . Num a => Num (u a)
      , forall a . Ord a => Ord (u a)
      , forall a . Real a => Real (u a)
      )
   => Unit (dim :: Type -> Type) u | u -> dim where
  prj :: u a -> a
  default prj :: Coercible (u a) a => u a -> a
  prj = coerce

  factor :: Floating a => K a (u a)
  factor = 1

  suffix :: K ShowS (u a)

instance Unit I I where
  suffix = K (showChar '1')

instance Unit Identity Identity where
  suffix = K (showChar '1')


-- ** Conversion

convert :: forall u u' d a . (Unit d u, Unit d u', Floating a) => u a -> u' a
convert = pure . (/ getK (factor @_ @u')) . (* getK (factor @_ @u)) . prj

converting :: forall u u' d a b . (Unit d u, Unit d u', Floating a, Floating b) => Iso (u a) (u b) (u' a) (u' b)
converting = iso convert convert


-- ** Comparison

(.==.) :: forall u u' d a . (Unit d u, Unit d u', Eq a, Floating a) => u a -> u' a -> Bool
a .==. b = prj a == prj (convert @u' @u b)

infix 4 .==.

compareU :: forall u u' d a . (Unit d u, Unit d u', Ord a, Floating a) => u a -> u' a -> Ordering
compareU a b = prj a `compare` prj (convert @u' @u b)

(.<.) :: (Unit d u, Unit d u', Ord a, Floating a) => u a -> u' a -> Bool
a .<. b = a `compareU` b == LT

infix 4 .<.

(.>.) :: (Unit d u, Unit d u', Ord a, Floating a) => u a -> u' a -> Bool
a .>. b = a `compareU` b == GT

infix 4 .>.

(.<=.) :: (Unit d u, Unit d u', Ord a, Floating a) => u a -> u' a -> Bool
a .<=. b = a `compareU` b /= GT

infix 4 .<=.

(.>=.) :: (Unit d u, Unit d u', Ord a, Floating a) => u a -> u' a -> Bool
a .>=. b = a `compareU` b /= LT

infix 4 .>=.


-- ** Formatting

formatWith :: forall u d a . Unit d u => (Maybe Int -> u a -> ShowS) -> Maybe Int -> u a -> ShowS
formatWith with n u = with n u . showChar ' ' . getK (suffix `asTypeOf` (u <$ K ('x':)))

format :: forall u d a . (Unit d u, RealFloat (u a)) => Maybe Int -> u a -> ShowS
format = formatWith showGFloat

formatDec :: forall u d a . (Unit d u, RealFloat (u a)) => Maybe Int -> u a -> ShowS
formatDec = formatWith showFFloat

formatExp :: forall u d a . (Unit d u, RealFloat (u a)) => Maybe Int -> u a -> ShowS
formatExp = formatWith showEFloat

formatExpR :: forall u d a . (Unit d u, RealFloat (u a)) => Maybe Int -> u a -> ShowS
formatExpR = formatWith (\ prec x -> if
  | isNaN x                   -> showString "NaN"
  | isInfinite x              -> showString $ if x < 0 then "-Infinity" else "Infinity"
  | x < 0 || isNegativeZero x -> showChar '-' . go prec (floatToDigits 10 (-x))
  | otherwise                 -> go prec (floatToDigits 10 x)) where
  go prec = \case
    ([0], _) -> showString "10⁰·0"
    (is,  e) | let is' = maybe is (\ ds -> digits (round (mul (take (ds + 1) is)))) prec
             -> showString "10" . superscript (e - 1) . showChar '·' . showDigits (take 1 is') . showChar '.' . showDigits (drop 1 is')
  showDigits = foldl' (\ s -> fmap s . showChar . intToDigit) id
  mul = foldl' (\ s d -> s * 10 + fromIntegral d) (0 :: Double)

digits :: Int -> [Int]
digits = go id where
  go s n | n >= 10   = let (q, r) = n `quotRem` 10 in go ((r:) . s) q
         | otherwise = n:s []

superscript :: Int -> ShowS
superscript i
  | signum i == -1 = ('⁻':) . go (abs i)
  | otherwise      = go i where
  go = foldl' (\ s -> fmap s . (:) . (sup !!)) id . digits
  sup = "⁰¹²³⁴⁵⁶⁷⁸⁹"


-- | A newtype for deriving 'Show' & 'Show1' from the behaviour of 'format', suitable for use with @-XDerivingVia@.
newtype Formatting u a = Formatting { getFormatting :: u a }

instance (Unit d u, RealFloat (u a)) => Show (Formatting u a) where
  showsPrec p = showParen (p > 10) . format Nothing . getFormatting
