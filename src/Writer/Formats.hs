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
  | SHORT
  | PSL

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
  "psl"     -> return PSL
  "basic"   -> return SHORT
  x         -> argsError ("Unknown format: " ++ x)
  
-----------------------------------------------------------------------------  
