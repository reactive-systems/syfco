-----------------------------------------------------------------------------
-- |
-- Module      :  Writer.Formats.Smv
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Leander Tentrup (tentrup@react.uni-saarland.de)
--                Felix Klein (klein@react.uni-saarland.de)
--
-- Transforms a specification to SMV format with decomposed formula.
-- See http://nusmv.fbk.eu/NuSMV/userman/v21/nusmv_3.html#SEC31 for more
-- information about the SMV LTL specification.
--
-----------------------------------------------------------------------------

module Writer.Formats.SmvDecomp where

-----------------------------------------------------------------------------

import Config
import Simplify

import Data.Error
import Data.List
import Data.Specification
import Data.LTL
    ( Formula(..)
    )

import Writer.Eval
import Writer.Data
import Writer.Utils

-----------------------------------------------------------------------------

-- | SMV LTL operator configuration.

opConfig
  :: OperatorConfig

opConfig = OperatorConfig
  { tTrue          = "TRUE"
  , fFalse         = "FALSE"
  , opNot          = UnaryOp  "!"   1
  , opAnd          = BinaryOp "&"   3 AssocLeft
  , opOr           = BinaryOp "|"   4 AssocLeft
  , opImplies      = BinaryOp "->"  6 AssocRight
  , opEquiv        = BinaryOp "<->" 5 AssocLeft
  , opNext         = UnaryOp  "X"   1
  , opStrongNext   = UnaryOp  "X"   1
  , opPrevious     = UnaryOpUnsupported
  , opFinally      = UnaryOp  "F"   1
  , opGlobally     = UnaryOp  "G"   1
  , opHistorically = UnaryOpUnsupported
  , opOnce         = UnaryOpUnsupported
  , opUntil        = BinaryOp "U"   2 AssocLeft
  , opRelease      = BinaryOp "V"   2 AssocLeft
  , opWeak         = BinaryOpUnsupported
  , opSince        = BinaryOpUnsupported
  , opTriggered    = BinaryOpUnsupported
  }

-----------------------------------------------------------------------------

-- | SMV LTL writer.

writeFormat
  :: Configuration -> Specification -> Either Error String

writeFormat config spec = do
  (es,ss,rs,as,is,gs) <- eval config spec

  fmlsI <- mapM (\i -> merge es ss rs as [i] []) is
  simpI <- mapM (simplify (adjust config opConfig)) fmlsI
  let flatI = concatMap flatten simpI
  strsI <- mapM (printFormula opConfig (outputMode config) (quoteMode config)) flatI
  fmlsG <- mapM (\g -> merge es ss rs as [] [g]) gs
  simpG <- mapM (simplify (adjust config opConfig)) fmlsG
  let flatG = concatMap flatten simpG
  strsG <- mapM (printFormula opConfig (outputMode config) (quoteMode config)) flatG

  (input_signals, output_signals) <- signals config spec
  let
    fml = (strsI ++ strsG)
    all_signals = (input_signals ++ output_signals)

  return $ main fml all_signals

  where
    flatten f = case f of
        And l -> l
        _ -> [f]

    main formulas xs =
        "MODULE main\n"
        ++ "\tVAR\n"
        ++ (printSignals xs)
        ++ intercalate "\n" (map (\f -> "\tLTLSPEC " ++ f) formulas)

    printSignals xs = case xs of
      []               -> ""
      (x:xr) -> "\t\t" ++ x ++ " : boolean;\n" ++ (printSignals xr)

-----------------------------------------------------------------------------

