module Isometry.UI
( UI(..)
) where

import UI.Label
import UI.Typeface

data UI = UI
  { target :: Label
  , face   :: Typeface
  }
