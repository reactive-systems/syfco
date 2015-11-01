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

-----------------------------------------------------------------------------

opNames
  :: OperatorNames

opNames = OperatorNames
  { opTrue = "TRUE" 
  , opFalse = "FALSE"
  , opNot = "!" 
  , opAnd = "*" 
  , opOr = "+" 
  , opImplies = "->" 
  , opEquiv = "<->" 
  , opNext = "X" 
  , opFinally = "F"
  , opGlobally = "G" 
  , opUntil = "U" 
  , opRelease = "R" 
  , opWeak = error "Wring does not support the weak until operator"
  }

-----------------------------------------------------------------------------

-- | Wring format writer.

writeWring
  :: Configuration -> Specification -> Either Error WriteContents

writeWring c s =
  let
    d = busDelimiter c
    mode = outputMode c
  in do
    (as,is,gs) <- eval d s
    fml0 <- merge as is gs
    fml1 <- simplify c $ adjust fml0
    
    return $ WriteContents {
      mainFile = pretty mode opNames fml1,
      partitionFile = Just $ partition (fmlInputs fml1) (fmlOutputs fml1)
      }

  where
    adjust fml = case fml of
      Not (Atomic (Output x)) -> Atomic (Output (x ++ "=0"))
      Not (Atomic (Input x))  -> Atomic (Input (x ++ "=0"))
      Atomic (Output x)       -> Atomic (Output (x ++ "=1"))            
      Atomic (Input x)        -> Atomic (Input (x ++ "=1"))
      _                       -> applySub adjust fml

-----------------------------------------------------------------------------    
