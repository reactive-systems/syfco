module Writer where

---

import Config

import Data.Error
import Data.Specification

import Writer.Data
import Writer.Formats

import Writer.Formats.Utf8
import Writer.Formats.Wring
import Writer.Formats.Promela
import Writer.Formats.Ltlxba
import Writer.Formats.Unbeast
import Writer.Formats.Basic
import Writer.Formats.Psl

---

writeSpecification
  :: Configuration -> Specification -> Either Error WriteContents

writeSpecification c s = case outputFormat c of
  UTF8    -> writeUtf8 c s
  SHORT   -> writeShort c s
  WRING   -> writeWring c s 
  LTLXBA  -> writeLtlxba c s 
  PROMELA -> writePromela c s
  UNBEAST -> writeUnbeast c s
  PSL     -> writePsl c s  

---
  
