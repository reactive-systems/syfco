-----------------------------------------------------------------------------
-- |
-- Module      :  Writer.Formats
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- Main list of supported writer formats.
--
-----------------------------------------------------------------------------

{-# LANGUAGE

    LambdaCase
  , MultiParamTypeClasses
  , TypeSynonymInstances
  , FlexibleInstances

  #-}

-----------------------------------------------------------------------------

module Writer.Formats
  ( WriteFormat(..)
  , needsLower
  ) where

-----------------------------------------------------------------------------

import Data.Convertible
  ( Convertible(..)
  , ConvertError(..)
  )

-----------------------------------------------------------------------------

-- | Supported writer formats.

data WriteFormat =
    UTF8
    -- ^ human readable output using UTF8 symbols
  | FULL
    -- ^ full format including all extensions
  | BASIC
    -- ^ basic format restricted to pure LTL formulas
  | WRING
    -- ^ <http://www.ist.tugraz.at/staff/bloem/wring.html>
  | PROMELA
    -- ^ <http://spinroot.com/spin/Man/ltl.html>
  | UNBEAST
    -- ^ <https://www.react.uni-saarland.de/tools/unbeast>
  | LTLXBA
    -- ^ LTL2BA / LTL3BA input format
  | LILY
    -- ^ Lily input format
  | ACACIA
    -- ^ Acacia / Acacia+ input format
  | ACACIASPECS
    -- ^ Acacia input format with spec units
  | SLUGS
    -- ^ <https://github.com/VerifiableRobotics/slugs/blob/master/doc/input_formats.md#structuredslugs>
  | SLUGSIN
    -- ^ <https://github.com/VerifiableRobotics/slugs/blob/master/doc/input_formats.md#slugsin>
  | PSL
    -- ^ <https://en.wikipedia.org/wiki/Property_Specification_Language>
  | SMV
    -- ^ SMV file format
  | BOSY
    -- ^ <https://github.com/reactive-systems/bosy>
  | RABINIZER
    -- ^ <https://www7.in.tum.de/~kretinsk/rabinizer3.html>
  deriving (Eq, Ord)

-----------------------------------------------------------------------------

-- | Human readable names of the formats, which may include spaces or
-- other special characters.

instance Show WriteFormat where
  show = \case
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
    RABINIZER   -> "Rabinizer"

-----------------------------------------------------------------------------

instance Convertible WriteFormat String where
  safeConvert = return . \case
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
    RABINIZER   -> "rabinizer"

-----------------------------------------------------------------------------

instance Convertible String WriteFormat where
  safeConvert = \case
    "utf8"         -> return UTF8
    "wring"        -> return WRING
    "promela"      -> return PROMELA
    "unbeast"      -> return UNBEAST
    "ltlxba"       -> return LTLXBA
    "lily"         -> return LILY
    "acacia"       -> return ACACIA
    "acacia-specs" -> return ACACIASPECS
    "basic"        -> return BASIC
    "slugs"        -> return SLUGS
    "slugsin"      -> return SLUGSIN
    "full"         -> return FULL
    "psl"          -> return PSL
    "smv"          -> return SMV
    "bosy"         -> return BOSY
    "rabinizer"    -> return RABINIZER
    str            -> Left ConvertError
      { convSourceValue = str
      , convSourceType = "String"
      , convDestType = "WriteFormat"
      , convErrorMessage = "Unknown format"
      }

-----------------------------------------------------------------------------

-- | Indicates the formats only support lower case signal names.

needsLower
  :: WriteFormat -> Bool

needsLower s =
  s `elem` [LTLXBA, PROMELA, ACACIA, ACACIASPECS, LILY, BOSY]

-----------------------------------------------------------------------------
