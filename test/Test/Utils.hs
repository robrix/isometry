module Test.Utils
( prop
) where

import GHC.Stack
import Hedgehog

prop :: HasCallStack => PropertyName -> PropertyT IO () -> (PropertyName, Property)
prop name = (,) name . property
