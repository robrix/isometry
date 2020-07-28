{-# LANGUAGE TemplateHaskell #-}
module Geometry.Transform.Test
( tests
) where

import Hedgehog

tests :: IO Bool
tests = checkParallel $$(discover)
