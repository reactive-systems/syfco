-----------------------------------------------------------------------------
-- |
-- Module      :  Writer.Formats.Ltlxba
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- Transforms a specification to the Ltl2ba / Ltl3ba format.
--
-----------------------------------------------------------------------------

module Writer.Formats.Ltlxba where

-----------------------------------------------------------------------------

import Config
import Simplify

import Data.Error
import Data.Specification

import Writer.Eval
import Writer.Data
import Writer.Utils

-----------------------------------------------------------------------------

-- | Ltl2ba / LTL3ba operator configuration.

opConfig
  :: OperatorConfig

opConfig = OperatorConfig
  { tTrue      = "true"
  , fFalse     = "false"
  , opNot      = UnaryOp  "!"   1
  , opAnd      = BinaryOp "&&"  4 AssocLeft
  , opOr       = BinaryOp "||"  4 AssocLeft
  , opImplies  = BinaryOp "->"  4 AssocLeft
  , opEquiv    = BinaryOp "<->" 4 AssocLeft
  , opNext     = UnaryOp  "X"   1
  , opFinally  = UnaryOp  "F"   1
  , opGlobally = UnaryOp  "G"   1
  , opUntil    = BinaryOp "U"   2 AssocLeft
  , opRelease  = BinaryOp "R"   3 AssocLeft
  , opWeak     = BinaryOpUnsupported
  }

-----------------------------------------------------------------------------

-- | Ltl2ba / LTL3ba writer.

writeFormat
  :: Configuration -> Specification -> Either Error String

writeFormat c s = do
  (es,ss,rs,as,is,gs) <- eval c s
  fml0 <- merge es ss rs as is gs
  fml1 <- simplify (adjust c opConfig) fml0

  return $ printFormula opConfig (outputMode c) fml1

-----------------------------------------------------------------------------
