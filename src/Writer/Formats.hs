-----------------------------------------------------------------------------
-- |
-- Module      :  Writer.Formats
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
-- 
-- Main list of supported writer formats.
-- 
-----------------------------------------------------------------------------

module Writer.Formats
    ( WriteFormat(..)
    , parseFormat
    , needsLower
    ) where

-----------------------------------------------------------------------------

import Data.Error
    ( Error
    , argsError
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
  | BASIC
  | SLUGS      
  | SLUGSIN
  | FULL  
  | PSL
  deriving (Eq)

-----------------------------------------------------------------------------

instance Show WriteFormat where
  show f = case f of
    UTF8    -> "Utf8"
    WRING   -> "Wring"
    PROMELA -> "Promela LTL"
    UNBEAST -> "Unbeast"
    LTLXBA  -> "LtlXba"
    LILY    -> "Lily"
    ACACIA  -> "Acacia"
    BASIC   -> "Basic"
    SLUGS   -> "Slugs"
    SLUGSIN -> "SlugsIn"
    FULL    -> "Full"
    PSL     -> "Psl"

-----------------------------------------------------------------------------

-- | Simple parser to read the corresponding format from the command line.

parseFormat
  :: String -> Either Error WriteFormat

parseFormat s = case s of
  "utf8"    -> return UTF8 
  "wring"   -> return WRING
  "ltlxba"  -> return LTLXBA
  "unbeast" -> return UNBEAST 
  "promela" -> return PROMELA
  "acacia"  -> return ACACIA
  "lily"    -> return LILY
  "psl"     -> return PSL
  "slugs"   -> return SLUGS
  "slugsin" -> return SLUGSIN
  "basic"   -> return BASIC
  "full"    -> return FULL
  x         -> argsError ("Unknown format: " ++ x)

-----------------------------------------------------------------------------

-- | Indicates the formats only support lower case signal names.

needsLower
  :: WriteFormat -> Bool

needsLower s = s `elem` [LTLXBA, PROMELA, ACACIA, LILY]

-----------------------------------------------------------------------------

