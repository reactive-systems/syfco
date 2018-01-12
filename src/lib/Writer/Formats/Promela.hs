-----------------------------------------------------------------------------
-- |
-- Module      :  Writer.Formats.Promela
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- Transforms a specification to Spins Promela LTL.
--
-----------------------------------------------------------------------------

module Writer.Formats.Promela where

-----------------------------------------------------------------------------

import Config
import Simplify

import Data.Error
import Data.Specification

import Writer.Eval
import Writer.Data
import Writer.Utils

-----------------------------------------------------------------------------

-- | Promela operator configuration.

opConfig
  :: OperatorConfig

opConfig = OperatorConfig
  { tTrue      = "true"
  , fFalse     = "false"
  , opNot      = UnaryOp  "!"   1
  , opAnd      = BinaryOp "&&"  3 AssocLeft
  , opOr       = BinaryOp "||"  3 AssocLeft
  , opImplies  = BinaryOp "->"  3 AssocLeft
  , opEquiv    = BinaryOp "<->" 3 AssocLeft
  , opNext     = UnaryOp  "X"   1
  , opFinally  = UnaryOp  "<>"  1
  , opGlobally = UnaryOp  "[]"  1
  , opUntil    = BinaryOp "U"   2 AssocLeft
  , opRelease  = BinaryOp "V"   2 AssocLeft
  , opWeak     = BinaryOpUnsupported
  }

-----------------------------------------------------------------------------

-- | Promela LTL writer.

writeFormat
  :: Configuration -> Specification -> Either Error String

writeFormat c s = do
  (es,ss,rs,as,is,gs) <- eval c s
  fml0 <- merge es ss rs as is gs
  fml1 <- simplify (adjust c opConfig) fml0

  return $ printFormula opConfig (outputMode c) fml1

-----------------------------------------------------------------------------
