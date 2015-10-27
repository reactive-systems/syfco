module Writer.Formats
       ( WriteFormat(..)
       , parseFormat
       ) where

---

import Data.Error

---

data WriteFormat =
    UTF8
  | WRING  
  | PROMELA
  | UNBEAST
  | LTLXBA  
  | SHORT
  | PSL

---    

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

---
