-----------------------------------------------------------------------------
-- |
-- Module      :  Writer.Formats.Wring
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
-- 
-- Transforms a specification to a wring format.
-- 
-----------------------------------------------------------------------------

module Writer.Formats.Wring where

-----------------------------------------------------------------------------

import Config
import Simplify

import Data.LTL
import Data.Error
import Data.Specification

import Writer.Eval
import Writer.Data
import Writer.Utils

-----------------------------------------------------------------------------

-- | Wring operator configuration.

opConfig
  :: OperatorConfig

opConfig = OperatorConfig
  { tTrue      = "TRUE"
  , fFalse     = "FALSE"
  , opNot      = UnaryOp  "!"   1
  , opAnd      = BinaryOp "*"   2 AssocLeft
  , opOr       = BinaryOp "+"   2 AssocLeft
  , opImplies  = BinaryOp "->"  2 AssocLeft
  , opEquiv    = BinaryOp "<->" 2 AssocLeft
  , opNext     = UnaryOp  "X"   1 
  , opFinally  = UnaryOp  "F"   1 
  , opGlobally = UnaryOp  "G"   1 
  , opUntil    = BinaryOp "U"   2 AssocLeft
  , opRelease  = BinaryOp "R"   2 AssocLeft
  , opWeak     = BinaryOpUnsupported
  }

-----------------------------------------------------------------------------

-- | Wring format writer.

writeFormat
  :: Configuration -> Specification -> Either Error String

writeFormat c s = do
  (es,ss,rs,as,is,gs) <- eval c s
  fml0 <- merge es ss rs as is gs
  fml1 <- simplify (adjust c opConfig) $ adjustAtomic fml0
    
  return $ printFormula opConfig (outputMode c) fml1

  where
    adjustAtomic fml = case fml of
      Not (Atomic (Output x)) -> Atomic (Output ("(" ++ x ++ "=0)"))
      Not (Atomic (Input x))  -> Atomic (Input ("(" ++ x ++ "=0)"))
      Atomic (Output x)       -> Atomic (Output ("(" ++ x ++ "=1)"))            
      Atomic (Input x)        -> Atomic (Input ("(" ++ x ++ "=1)"))
      _                       -> applySub adjustAtomic fml    

-----------------------------------------------------------------------------    
