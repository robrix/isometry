{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
module Starlight.CLI
( Options(..)
, defaultOptions
, profile_
, trace_
, check_
, argumentsParser
  -- * Re-exports
, execParser
) where

import           Control.Lens
import           Data.Foldable (foldl')
import           Data.Generics.Product.Fields
import           Data.Version (showVersion)
import           GHC.Generics (Generic)
import           Options.Applicative
import qualified Paths_starlight as Library (version)

data Options = Options
  { profile :: Bool
  , trace   :: Bool
  , check   :: Bool
  }
  deriving (Generic, Show)

defaultOptions :: Options
defaultOptions = Options
  { profile = False
  , trace   = False
  , check   = False
  }

profile_ :: Lens' Options Bool
profile_ = field @"profile"

trace_ :: Lens' Options Bool
trace_ = field @"trace"

check_ :: Lens' Options Bool
check_ = field @"check"


argumentsParser :: ParserInfo Options
argumentsParser = info
  (version <*> helper <*> options)
  (  fullDesc
  <> progDesc "Starlight is a game about spaceships in space."
  <> header   "Starlight - spaceships in space")

options :: Parser Options
options = foldl' (&) defaultOptions <$> sequenceA
  [ flag id (profile_ .~ True) (long "profile" <> help "run with profiling enabled")
  , flag id (trace_   .~ True) (long "trace"   <> help "run with tracing enabled")
  , flag id (check_   .~ True) (long "check"   <> help "run with error checking enabled")
  ]


versionString :: String
versionString = "starlight version " <> showVersion Library.version

version :: Parser (a -> a)
version = infoOption versionString (long "version" <> short 'V' <> help "Output version info.")
