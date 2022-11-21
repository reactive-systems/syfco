-----------------------------------------------------------------------------
-- |
-- Module      :  Writer.Formats.Utf8
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- Transforms a specification to a UTF8 string.
--
-----------------------------------------------------------------------------

module Writer.Formats.Utf8 where

-----------------------------------------------------------------------------

import Config
import Simplify

import Data.Error
import Data.Specification

import Writer.Eval
import Writer.Data
import Writer.Utils

-----------------------------------------------------------------------------

-- | UTF8 operator configuration.

opConfig
  :: OperatorConfig

opConfig = OperatorConfig
  { tTrue          = "⊤"
  , fFalse         = "⊥"
  , opNot          = UnaryOp  "¬" 1
  , opAnd          = BinaryOp "∧" 2 AssocLeft
  , opOr           = BinaryOp "∨" 3 AssocLeft
  , opImplies      = BinaryOp "→" 4 AssocRight
  , opEquiv        = BinaryOp "↔" 4 AssocRight
  , opNext         = UnaryOp  "◯" 1
  , opStrongNext   = UnaryOp  "◯" 1
  , opPrevious     = UnaryOp  "◉" 1
  , opFinally      = UnaryOp  "◇" 1
  , opGlobally     = UnaryOp  "▢" 1
  , opHistorically = UnaryOp  "▣" 1
  , opOnce         = UnaryOp  "◈" 1
  , opUntil        = BinaryOp "U" 6 AssocRight
  , opRelease      = BinaryOp "R" 7 AssocLeft
  , opWeak         = BinaryOp "W" 5 AssocRight
  , opSince        = BinaryOp "S" 8 AssocRight
  , opTriggered    = BinaryOp "T" 9 AssocLeft
  }

-----------------------------------------------------------------------------

-- | UTF8 writer.

writeFormat
  :: Configuration -> Specification -> Either Error String

writeFormat c s = do
  (es,ss,rs,as,is,gs) <- eval c s
  fml0 <- merge es ss rs as is gs
  fml1 <- simplify (adjust c opConfig) fml0

  printFormula opConfig (outputMode c) (quoteMode c) fml1

-----------------------------------------------------------------------------
