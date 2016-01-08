-----------------------------------------------------------------------------
-- |
-- Module      :  Writer.Formats.Utf8
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
-- 
-- Transforms a specification to a UTF8 string.
-- 
-----------------------------------------------------------------------------

module Writer.Formats.Utf8
    ( writeUtf8
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

opConfig
  :: OperatorConfig

opConfig = OperatorConfig
  { tTrue      = "⊤"
  , fFalse     = "⊥"
  , opNot      = UnaOp "¬"   1
  , opAnd      = BinOp "∧"   2 AssocLeft
  , opOr       = BinOp "∨"   3 AssocLeft
  , opImplies  = BinOp "→"   4 AssocRight
  , opEquiv    = BinOp "↔"   4 AssocRight
  , opNext     = UnaOp "◯"   1 
  , opFinally  = UnaOp "◇"   1 
  , opGlobally = UnaOp "□"   1 
  , opUntil    = BinOp "U"   6 AssocRight
  , opRelease  = BinOp "R"   7 AssocLeft
  , opWeak     = BinOp "W"   5 AssocRight
  }

-----------------------------------------------------------------------------

-- | UTF8 writer.

writeUtf8
  :: Configuration -> Specification -> Either Error String

writeUtf8 c s = do
  (as,is,gs) <- eval c s
  fml0 <- merge as is gs
  fml1 <- simplify c fml0
    
  return $ printFormula opConfig (outputMode c) fml1

-----------------------------------------------------------------------------

