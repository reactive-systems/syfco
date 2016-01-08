-----------------------------------------------------------------------------
-- |
-- Module      :  Writer.Formats.Utf8
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
-- 
-- Transforms a specification to a UTF8 string.
-- 
-----------------------------------------------------------------------------

module Writer.Formats.Psl
    ( writePsl
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
  , opNot      = UnaOp "!"           1
  , opAnd      = BinOp "&&"          2 AssocLeft
  , opOr       = BinOp "||"          3 AssocLeft
  , opImplies  = BinOp "->"          6 AssocRight
  , opEquiv    = BinOp "<->"         6 AssocRight
  , opNext     = UnaOp "next!"       4 
  , opFinally  = UnaOp "eventually!" 4 
  , opGlobally = UnaOp "always"      7      
  , opUntil    = BinOp "until!"      5 AssocRight
  , opRelease  = assert False undefined                                  
  , opWeak     = assert False undefined                 

  }

-----------------------------------------------------------------------------

-- | PSL format writer.

writePsl
  :: Configuration -> Specification -> Either Error String

writePsl c s = do
  (as,is,gs) <- eval c s
  fml0 <- merge as is gs
  fml1 <- simplify (c { noWeak = True, noRelease = True }) fml0
  
  return $ printFormula opConfig (outputMode c) fml1

-----------------------------------------------------------------------------

