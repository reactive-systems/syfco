-----------------------------------------------------------------------------
-- |
-- Module      :  Writer.Formats.Ltlxba
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
-- 
-- Transforms a specification to the Ltl2ba / Ltl3ba format.
-- 
-----------------------------------------------------------------------------

module Writer.Formats.Ltlxba
    ( writeLtlxba
    ) where

-----------------------------------------------------------------------------

import Config
import Simplify

import Data.Error
import Data.Specification

import Writer.Eval
import Writer.Data
import Writer.Utils

-----------------------------------------------------------------------------

opNames
  :: OperatorNames

opNames = OperatorNames
  { opTrue = "true" 
  , opFalse = "false"
  , opNot = "!"
  , opAnd = "&&" 
  , opOr = "||" 
  , opImplies = "->" 
  , opEquiv = "<->" 
  , opNext = "X" 
  , opFinally = "F"
  , opGlobally = "G" 
  , opUntil = "U" 
  , opRelease = "R"
  , opWeak = error "LtlXBa does not support the weak until operator"
  }

-----------------------------------------------------------------------------

-- | Ltl2ba / LTL3ba writer.

writeLtlxba
  :: Configuration -> Specification -> Either Error String

writeLtlxba c s = do
  (as,is,gs) <- eval c s
  fml0 <- merge as is gs
  fml1 <- simplify (c { noWeak = True }) fml0
    
  return $ pretty (outputMode c) opNames fml1

-----------------------------------------------------------------------------


