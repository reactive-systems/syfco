-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Info
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- Information shared by different modules.
--
-----------------------------------------------------------------------------

module Data.Info
  ( name
  , version
  , defaultDelimiter
  , defaultPrimeSymbol
  , defaultAtSymbol
  ) where

-----------------------------------------------------------------------------

import qualified Paths_syfco as P
  ( version
  )

import Data.Version
  ( Version(..)
  , showVersion
  )

import Data.Char
  ( toLower
  )

-----------------------------------------------------------------------------

-- | The default delimiter symbol

defaultDelimiter
  :: String

defaultDelimiter = "_"

-----------------------------------------------------------------------------

-- | The default prime symbol

defaultPrimeSymbol
  :: String

defaultPrimeSymbol = "'"


-----------------------------------------------------------------------------

-- | The default at symbol

defaultAtSymbol
  :: String

defaultAtSymbol = "@"

-----------------------------------------------------------------------------

-- | The name of the tool.

name
  :: String

name = "SyFCo"

-----------------------------------------------------------------------------

-- | Returns the build version of the library. Requires the library to
-- be built with cabal or stack.

version = case P.version of
  Version [0,0,0,0] [] -> "no version information available"
  _                    -> showVersion P.version

-----------------------------------------------------------------------------
