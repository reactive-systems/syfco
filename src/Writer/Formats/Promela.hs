-----------------------------------------------------------------------------
-- |
-- Module      :  Writer.Formats.Promela
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
-- 
-- Transforms a specification to Spins Promela LTL.
-- 
-----------------------------------------------------------------------------

module Writer.Formats.Promela
    ( writePromela
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
  , opAnd      = BinOp "&&"  3 AssocLeft
  , opOr       = BinOp "||"  3 AssocLeft
  , opImplies  = BinOp "->"  3 AssocLeft
  , opEquiv    = BinOp "<->" 3 AssocLeft
  , opNext     = UnaOp "X"   1 
  , opFinally  = UnaOp "<>"  1 
  , opGlobally = UnaOp "[]"  1  
  , opUntil    = BinOp "U"   2 AssocLeft 
  , opRelease  = BinOp "V"   2 AssocLeft 
  , opWeak     = assert False undefined
  }

-----------------------------------------------------------------------------

-- | Promela LTL writer.

writePromela
  :: Configuration -> Specification -> Either Error String

writePromela c s = do
  (as,is,gs) <- eval c s
  fml0 <- merge as is gs
  fml1 <- simplify (c { noWeak = True }) fml0
    
  return $ printFormula opConfig (outputMode c) fml1  

-----------------------------------------------------------------------------


         
