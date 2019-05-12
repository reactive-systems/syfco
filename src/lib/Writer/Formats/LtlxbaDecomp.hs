-----------------------------------------------------------------------------
-- |
-- Module      :  Writer.Formats.LtlxbaDecomp
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Guillermo A. Perez (guillermoalberto.perez@uantwerpen.be)
--                Felix Klein (klein@react.uni-saarland.de)
--
-- Transforms a specification to a conjunction of Ltl2ba / Ltl3ba formulas.
--
-----------------------------------------------------------------------------

module Writer.Formats.LtlxbaDecomp where

-----------------------------------------------------------------------------

import Config
import Simplify

import Data.Error
import Data.List
import Data.Specification

import Writer.Eval
import Writer.Data
import Writer.Utils

-----------------------------------------------------------------------------

-- | Ltl2ba / LTL3ba operator configuration.

opConfig
  :: OperatorConfig

opConfig = OperatorConfig
  { tTrue          = "true"
  , fFalse         = "false"
  , opNot          = UnaryOp  "!"   1
  , opAnd          = BinaryOp "&&"  4 AssocLeft
  , opOr           = BinaryOp "||"  4 AssocLeft
  , opImplies      = BinaryOp "->"  4 AssocLeft
  , opEquiv        = BinaryOp "<->" 4 AssocLeft
  , opNext         = UnaryOp  "X"   1
  , opPrevious     = UnaryOpUnsupported
  , opFinally      = UnaryOp  "F"   1
  , opGlobally     = UnaryOp  "G"   1
  , opHistorically = UnaryOpUnsupported
  , opOnce         = UnaryOpUnsupported
  , opUntil        = BinaryOp "U"   2 AssocLeft
  , opRelease      = BinaryOp "R"   3 AssocLeft
  , opWeak         = BinaryOpUnsupported
  , opSince        = BinaryOpUnsupported
  , opTriggered    = BinaryOpUnsupported
  }

-----------------------------------------------------------------------------

-- | Ltl2ba / LTL3ba writer.

writeFormat
  :: Configuration -> Specification -> Either Error String

writeFormat c s = do
  (es,ss,rs,as,is,gs) <- eval c s
  fmlsI <- mapM (\i -> merge es ss rs as [i] []) is
  simpI <- mapM (simplify (adjust c opConfig)) fmlsI
  strsI <- mapM (printFormula opConfig (outputMode c)) simpI
  fmlsG <- mapM (\g -> merge es ss rs as [] [g]) gs
  simpG <- mapM (simplify (adjust c opConfig)) fmlsG
  strsG <- mapM (printFormula opConfig (outputMode c)) simpG
  return $ intercalate "\n" (strsI ++ strsG)

-----------------------------------------------------------------------------
