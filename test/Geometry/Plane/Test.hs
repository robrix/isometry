{-# LANGUAGE TemplateHaskell #-}
module Geometry.Plane.Test
( tests
) where

import Hedgehog

tests :: IO Bool
tests = checkParallel $$(discover)
