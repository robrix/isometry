module Isometry.Language
( Time(..)
) where

class Time expr where
  time :: expr Double
