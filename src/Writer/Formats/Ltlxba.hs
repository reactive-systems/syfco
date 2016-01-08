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

import Control.Exception

-----------------------------------------------------------------------------

opConfig
  :: OperatorConfig

opConfig = OperatorConfig
  { tTrue      = "true"
  , fFalse     = "false"
  , opNot      = UnaOp "!"   1
  , opAnd      = BinOp "&&"  4 AssocLeft
  , opOr       = BinOp "||"  4 AssocLeft
  , opImplies  = BinOp "->"  4 AssocLeft
  , opEquiv    = BinOp "<->" 4 AssocLeft
  , opNext     = UnaOp "X"   1 
  , opFinally  = UnaOp "F"   1 
  , opGlobally = UnaOp "G"   1 
  , opUntil    = BinOp "U"   2 AssocLeft
  , opRelease  = BinOp "R"   3 AssocLeft
  , opWeak     = assert False undefined
  }

-----------------------------------------------------------------------------

-- | Ltl2ba / LTL3ba writer.

writeLtlxba
  :: Configuration -> Specification -> Either Error String

writeLtlxba c s = do
  (as,is,gs) <- eval c s
  fml0 <- merge as is gs
  fml1 <- simplify (c { noWeak = True }) fml0
    
  return $ printFormula opConfig (outputMode c) fml1

-----------------------------------------------------------------------------


