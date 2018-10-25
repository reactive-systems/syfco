-----------------------------------------------------------------------------
-- |
-- Module      :  Writer.Formats.Utf8
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- Transforms a specification to a UTF8 string.
--
-----------------------------------------------------------------------------

module Writer.Formats.Psl where

-----------------------------------------------------------------------------

import Config
import Simplify

import Data.Error
import Data.Specification

import Writer.Eval
import Writer.Data
import Writer.Utils

-----------------------------------------------------------------------------

-- | PSL operator configuration.

opConfig
  :: OperatorConfig

opConfig = OperatorConfig
  { tTrue          = "true"
  , fFalse         = "false"
  , opNot          = UnaryOp  "!"           1
  , opAnd          = BinaryOp "&&"          2 AssocLeft
  , opOr           = BinaryOp "||"          3 AssocLeft
  , opImplies      = BinaryOp "->"          6 AssocRight
  , opEquiv        = BinaryOp "<->"         6 AssocRight
  , opNext         = UnaryOp  "next!"       4
  , opPrevious     = UnaryOpUnsupported
  , opFinally      = UnaryOp  "eventually!" 4
  , opGlobally     = UnaryOp  "always"      7
  , opHistorically = UnaryOpUnsupported
  , opOnce         = UnaryOpUnsupported
  , opUntil        = BinaryOp "until!"      5 AssocRight
  , opRelease      = BinaryOpUnsupported
  , opWeak         = BinaryOpUnsupported
  , opSince        = BinaryOpUnsupported
  , opTriggered    = BinaryOpUnsupported
  }

-----------------------------------------------------------------------------

-- | PSL format writer.

writeFormat
  :: Configuration -> Specification -> Either Error String

writeFormat c s = do
  (es,ss,rs,as,is,gs) <- eval c s
  fml0 <- merge es ss rs as is gs
  fml1 <- simplify (adjust c opConfig) fml0

  printFormula opConfig (outputMode c) fml1

-----------------------------------------------------------------------------
