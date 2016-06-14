-----------------------------------------------------------------------------
-- |
-- Module      :  Writer.Formats.Smv
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Leander Tentrup (tentrup@react.uni-saarland.de)
--                Felix Klein (klein@react.uni-saarland.de)
-- 
-- Transforms a specification to SMV format.
-- See http://nusmv.fbk.eu/NuSMV/userman/v21/nusmv_3.html#SEC31 for more
-- information about the SMV LTL specification.
-- 
-----------------------------------------------------------------------------

module Writer.Formats.Smv where

-----------------------------------------------------------------------------

import Config
import Simplify

import Data.Error
import Data.Specification

import Writer.Eval
import Writer.Data
import Writer.Utils

-----------------------------------------------------------------------------

-- | SMV LTL operator configuration.

opConfig
  :: OperatorConfig

opConfig = OperatorConfig
  { tTrue      = "TRUE"
  , fFalse     = "FALSE"
  , opNot      = UnaryOp  "!"   1
  , opAnd      = BinaryOp "&"   3 AssocLeft
  , opOr       = BinaryOp "|"   4 AssocLeft
  , opImplies  = BinaryOp "->"  6 AssocRight
  , opEquiv    = BinaryOp "<->" 5 AssocLeft
  , opNext     = UnaryOp  "X"   1
  , opFinally  = UnaryOp  "F"   1
  , opGlobally = UnaryOp  "G"   1
  , opUntil    = BinaryOp "U"   2 AssocLeft
  , opRelease  = BinaryOp "V"   2 AssocLeft
  , opWeak     = BinaryOpUnsupported
  }

-----------------------------------------------------------------------------

-- | SMV LTL writer.

writeFormat
  :: Configuration -> Specification -> Either Error String

writeFormat config spec = do
  (es,ss,rs,as,is,gs) <- eval config spec
  formula <- merge es ss rs as is gs
  simplified_formula <- simplify (adjust config opConfig) formula
    
  
  (input_signals, output_signals) <- evalSignals config spec
  let 
    signals = (input_signals ++ output_signals)
  
  return $ main (printFormula opConfig (outputMode config) simplified_formula) signals
  
  where 
    main formula signals =
        "MODULE main\n"
        ++ "\tVAR\n"
        ++ (printSignals signals)
        ++ "\tLTLSPEC " ++ formula ++ "\n"

    printSignals signals = case signals of
      []               -> ""
      (signal:signals) -> "\t\t" ++ signal ++ " : boolean;\n" ++ (printSignals signals)

-----------------------------------------------------------------------------


         
