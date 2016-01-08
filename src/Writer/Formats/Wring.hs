-----------------------------------------------------------------------------
-- |
-- Module      :  Writer.Formats.Wring
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
-- 
-- Transforms a specification to a wring format.
-- 
-----------------------------------------------------------------------------

module Writer.Formats.Wring
    ( writeWring
    ) where

-----------------------------------------------------------------------------

import Config
import Simplify

import Data.LTL
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
  { tTrue      = "TRUE"
  , fFalse     = "FALSE"
  , opNot      = UnaOp "!"   1
  , opAnd      = BinOp "*"   2 AssocLeft
  , opOr       = BinOp "+"   2 AssocLeft
  , opImplies  = BinOp "->"  2 AssocLeft
  , opEquiv    = BinOp "<->" 2 AssocLeft
  , opNext     = UnaOp "X"   1 
  , opFinally  = UnaOp "F"   1 
  , opGlobally = UnaOp "G"   1 
  , opUntil    = BinOp "U"   2 AssocLeft
  , opRelease  = BinOp "R"   2 AssocLeft
  , opWeak     = assert False undefined
  }

-----------------------------------------------------------------------------

-- | Wring format writer.

writeWring
  :: Configuration -> Specification -> Either Error String

writeWring c s = do
  (as,is,gs) <- eval c s
  fml0 <- merge as is gs
  fml1 <- simplify (c { noWeak = True }) $ adjust fml0
    
  return $ printFormula opConfig (outputMode c) fml1

  where
    adjust fml = case fml of
      Not (Atomic (Output x)) -> Atomic (Output (x ++ "=0"))
      Not (Atomic (Input x))  -> Atomic (Input (x ++ "=0"))
      Atomic (Output x)       -> Atomic (Output (x ++ "=1"))            
      Atomic (Input x)        -> Atomic (Input (x ++ "=1"))
      _                       -> applySub adjust fml

-----------------------------------------------------------------------------    
