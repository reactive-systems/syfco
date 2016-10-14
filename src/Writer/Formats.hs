-----------------------------------------------------------------------------
-- |
-- Module      :  Writer.Formats
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- Main list of supported writer formats.
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric #-}

-----------------------------------------------------------------------------

module Writer.Formats
  ( WriteFormat(..)
  , needsLower
  ) where

-----------------------------------------------------------------------------

import Data.Utils
  ( MachinePrintable(..)
  )

import Data.List
  ( find
  )

import Data.Error
  ( Error
  , argsError
  )

import GHC.Generics
  ( Generic
  )

import Generics.Deriving.Enum
  ( GEnum
  )

-----------------------------------------------------------------------------

-- | Supported writer formats.

data WriteFormat =
    UTF8
  | WRING
  | PROMELA
  | UNBEAST
  | LTLXBA
  | LILY
  | ACACIA
  | ACACIASPECS
  | BASIC
  | SLUGS
  | SLUGSIN
  | FULL
  | PSL
  | SMV
  | BOSY
  deriving (Eq, Generic)

instance GEnum WriteFormat

-----------------------------------------------------------------------------

-- | Human readable names of the formats, which may include spaces or
-- other special characters.

instance Show WriteFormat where
  show f = case f of
    UTF8        -> "Utf8"
    WRING       -> "Wring"
    PROMELA     -> "Promela LTL"
    UNBEAST     -> "Unbeast"
    LTLXBA      -> "LtlXba"
    LILY        -> "Lily"
    ACACIA      -> "Acacia"
    ACACIASPECS -> "AcaciaSpecs"
    BASIC       -> "Basic"
    SLUGS       -> "Slugs"
    SLUGSIN     -> "SlugsIn"
    FULL        -> "Full"
    PSL         -> "Psl"
    SMV         -> "SMV"
    BOSY        -> "BoSy"

-----------------------------------------------------------------------------

-- | Computer readable names of the formats used via the command line or
-- configuration files. The name of each format has to be unique.

instance MachinePrintable WriteFormat where
  mprint fmt = case fmt of
    UTF8        -> "utf8"
    WRING       -> "wring"
    PROMELA     -> "promela"
    UNBEAST     -> "unbeast"
    LTLXBA      -> "ltlxba"
    LILY        -> "lily"
    ACACIA      -> "acacia"
    ACACIASPECS -> "acacia-specs"
    BASIC       -> "basic"
    SLUGS       -> "slugs"
    SLUGSIN     -> "slugsin"
    FULL        -> "full"
    PSL         -> "psl"
    SMV         -> "smv"
    BOSY        -> "bosy"

-----------------------------------------------------------------------------

-- | Indicates the formats only support lower case signal names.

needsLower
  :: WriteFormat -> Bool

needsLower s = s `elem` [LTLXBA, PROMELA, ACACIA, ACACIASPECS, LILY, BOSY]

-----------------------------------------------------------------------------
