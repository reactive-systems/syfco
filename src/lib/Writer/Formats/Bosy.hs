-----------------------------------------------------------------------------
-- |
-- Module      :  Writer.Formats.Bosy
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Leander Tentrup (tentrup@react.uni-saarland.de)
--                Felix Klein (klein@react.uni-saarland.de)
--
-- Transforms a specification to the BoSy JSON file format
--
-----------------------------------------------------------------------------

module Writer.Formats.Bosy where

-----------------------------------------------------------------------------

import Config
import Simplify

import Data.List
import Data.LTL
import Data.Types
import Data.Error
import Data.Specification

import Writer.Eval
import Writer.Data
import Writer.Utils

import Data.Char( toLower )

-----------------------------------------------------------------------------

-- | Basic Format operator configuration.

opConfig
  :: OperatorConfig

opConfig = OperatorConfig
  { tTrue          = "true"
  , fFalse         = "false"
  , opNot          = UnaryOp "!"   1
  , opAnd          = BinaryOp "&&"  2 AssocLeft
  , opOr           = BinaryOp "||"  3 AssocLeft
  , opImplies      = BinaryOp "->"  4 AssocRight
  , opEquiv        = BinaryOp "<->" 4 AssocRight
  , opNext         = UnaryOp  "X"   1
  , opStrongNext   = UnaryOpUnsupported
  , opWeakNext     = UnaryOpUnsupported
  , opPrevious     = UnaryOpUnsupported
  , opFinally      = UnaryOp  "F"   1
  , opGlobally     = UnaryOp  "G"   1
  , opHistorically = UnaryOpUnsupported
  , opOnce         = UnaryOpUnsupported
  , opUntil        = BinaryOp "U"   6 AssocRight
  , opRelease      = BinaryOp "R"   7 AssocLeft
  , opWeak         = BinaryOpUnsupported
  , opSince        = BinaryOpUnsupported
  , opTriggered    = BinaryOpUnsupported
  }

-----------------------------------------------------------------------------

-- | Bosy JSON writer.

writeFormat
  :: Configuration -> Specification -> Either Error String

writeFormat config specification = do
  let config' = config {
      simplifyStrong = True
  }

  (initial, preset, requirments, assumptions, assertions, guarantees) <- eval config' specification
  _ <- mapM (simplify (adjust config' opConfig)) initial
  _ <- mapM (simplify (adjust config' opConfig)) preset
  requirments' <- mapM ((simplify (adjust config' opConfig)) . fGlobally) requirments
  assumptions' <- mapM (simplify (adjust config' opConfig)) assumptions
  assertions' <- mapM ((simplify (adjust config' opConfig)) . fGlobally) assertions
  guarantees' <- mapM (simplify (adjust config' opConfig)) guarantees

  (inputs, outputs) <- signals config' specification

  assumptions'' <- mapM (printFormula opConfig Fully NoQuotes) (requirments' ++ assumptions')
  guarantees'' <- mapM (printFormula opConfig  Fully NoQuotes) (assertions' ++ guarantees')

  return $
    "{" ++
    "\"semantics\": " ++
      (case owSemantics config of
         Nothing       -> printSemantics (semantics specification)
         Just x        -> printSemantics x) ++ ", " ++
    "\"inputs\": [" ++ (intercalate ", " (map (quote . map toLower) inputs)) ++ "], " ++
    "\"outputs\": [" ++ (intercalate ", " (map (quote . map toLower) outputs)) ++ "], " ++
    "\"assumptions\": [" ++ (intercalate ", " (map quote assumptions'')) ++ "], " ++
    "\"guarantees\": [" ++ (intercalate ", " (map quote guarantees'')) ++ "] " ++
    "}\n"

  where
    printSemantics specSemantics =
      case specSemantics of
        SemanticsMealy       -> "\"mealy\""
        SemanticsMoore       -> "\"moore\""
        SemanticsStrictMealy -> "\"mealy\""
        SemanticsStrictMoore -> "\"moore\""
        SemanticsFiniteMealy -> "\"mealy\""
        SemanticsFiniteMoore -> "\"moore\""

    quote x = "\"" ++ x ++ "\""

-----------------------------------------------------------------------------
