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
  { opTrue = "true"
  , opFalse = "false"
  , opNot = "!"
  , opAnd = "&&"
  , opOr = "||"
  , opImplies = "->"
  , opEquiv = "<->"
  , opNext = "X"
  , opFinally = "<>"
  , opGlobally = "[]"
  , opUntil = "U"
  , opRelease = "V"
  , opWeak = "W"  
  }

-----------------------------------------------------------------------------

-- | Promela LTL writer.

writePromela
  :: Configuration -> Specification -> Either Error WriteContents

writePromela c s =
  let
    d = busDelimiter c
    mode = outputMode c
  in do
    (as,is,gs) <- eval d s
    fml0 <- merge as is gs
    fml1 <- simplify c fml0
    
    return $ WriteContents {
      mainFile = pretty mode opNames fml1,
      partitionFile = Just $ partition (fmlInputs fml1) (fmlOutputs fml1)
      }

-----------------------------------------------------------------------------


         
