-----------------------------------------------------------------------------
-- |
-- Module      :  Writer.Formats.Basic
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- Transforms a specification to the basic TLSF version without high level
-- constructs.
--
-----------------------------------------------------------------------------

module Writer.Formats.Basic where

-----------------------------------------------------------------------------

import Config
import Simplify

import Data.Maybe
import Data.LTL
import Data.Types
import Data.Error
import Data.Specification

import Writer.Eval
import Writer.Data
import Writer.Utils

-----------------------------------------------------------------------------

-- | Basic Format operator configuration.

opConfig
  :: OperatorConfig

opConfig = OperatorConfig
  { tTrue          = "true"
  , fFalse         = "false"
  , opNot          = UnaryOp "!"    1
  , opAnd          = BinaryOp "&&"  2 AssocLeft
  , opOr           = BinaryOp "||"  3 AssocLeft
  , opImplies      = BinaryOp "->"  4 AssocRight
  , opEquiv        = BinaryOp "<->" 4 AssocRight
  , opNext         = UnaryOp  "X"   1
  , opStrongNext   = UnaryOpUnsupported
  , opWeakNext     = UnaryOpUnsupported
  , opPrevious     = UnaryOp  "Y"   1
  , opFinally      = UnaryOp  "F"   1
  , opGlobally     = UnaryOp  "G"   1
  , opHistorically = UnaryOp  "H"   1
  , opOnce         = UnaryOp  "O"   1
  , opUntil        = BinaryOp "U"   6 AssocRight
  , opRelease      = BinaryOp "R"   7 AssocLeft
  , opWeak         = BinaryOp "W"   5 AssocRight
  , opSince        = BinaryOp "S"   8 AssocRight
  , opTriggered    = BinaryOp "T"   9 AssocLeft
  }

-----------------------------------------------------------------------------

-- | Basic TLSF writer.

writeFormat
  :: Configuration -> Specification -> Either Error String

writeFormat c s = do
  let s' = s {
      target = fromMaybe (target s) $ owTarget c,
      semantics = fromMaybe (semantics s) $ owSemantics c
      }

  (es,ss,rs,as,is,gs) <- eval c s

  es' <- mapM (simplify (adjust c opConfig)) es
  ss' <- mapM (simplify (adjust c opConfig)) ss
  rs' <- mapM (simplify (adjust c opConfig)) rs
  as' <- mapM (simplify (adjust c opConfig)) as
  is' <- mapM (simplify (adjust c opConfig)) is
  gs' <- mapM (simplify (adjust c opConfig)) gs

  es'' <- mapM (printFormula opConfig Fully NoQuotes) $ filter checkTrue es'
  ss'' <- mapM (printFormula opConfig Fully NoQuotes) $ filter checkTrue ss'
  rs'' <- mapM (printFormula opConfig Fully NoQuotes) $ filter checkTrue rs'
  as'' <- mapM (printFormula opConfig Fully NoQuotes) $ filter checkTrue as'
  is'' <- mapM (printFormula opConfig Fully NoQuotes) $ filter checkTrue is'
  gs'' <- mapM (printFormula opConfig Fully NoQuotes) $ filter checkTrue gs'

  (si,so) <- signals c s'

  return $
    "INFO {"
    ++ "\n" ++ "  TITLE:       \"" ++ title s' ++ "\""
    ++ "\n" ++ "  DESCRIPTION: \"" ++ description s' ++ "\""
    ++ "\n" ++ "  SEMANTICS:   " ++
      (case semantics s' of
         SemanticsMealy       -> "Mealy"
         SemanticsMoore       -> "Moore"
         SemanticsStrictMealy -> "Strict,Mealy"
         SemanticsStrictMoore -> "Strict,Moore"
         SemanticsFiniteMealy -> "Finite,Mealy"
         SemanticsFiniteMoore -> "Finite,Moore")
    ++ "\n" ++ "  TARGET:      " ++
      (case target s' of
         TargetMealy -> "Mealy"
         TargetMoore -> "Moore")
    ++ (if null $ tags s' then ""
        else "\n  TAGS:        " ++ head (tags s') ++
             concatMap ((:) ' ' . (:) ',') (tail $ tags s'))
    ++ "\n" ++ "}"
    ++ "\n"
    ++ "\n" ++ "MAIN {"
    ++ "\n" ++ "  INPUTS {"
    ++ concatMap printSignal si
    ++ "\n" ++ "  }"
    ++ "\n" ++ "  OUTPUTS {"
    ++ concatMap printSignal so
    ++ "\n" ++ "  }"
    ++ (if not $ any checkTrue es' then ""
        else "\n" ++ "  INITIALLY {" ++ pr es'' ++ "\n" ++ "  }")
    ++ (if not $ any checkTrue ss' then ""
        else "\n" ++ "  PRESET {" ++ pr ss'' ++ "\n" ++ "  }")
    ++ (if not $ any checkTrue rs' then ""
        else "\n" ++ "  REQUIRE {" ++ pr rs'' ++ "\n" ++ "  }")
    ++ (if not $ any checkTrue as' then ""
        else "\n" ++ "  ASSUME {" ++ pr as'' ++"\n" ++ "  }")
    ++ (if not $ any checkTrue is' then ""
        else "\n" ++ "  ASSERT {" ++ pr is'' ++ "\n" ++ "  }")
    ++ (if not $ any checkTrue gs' then ""
        else "\n" ++ "  GUARANTEE {" ++ pr gs'' ++ "\n" ++ "  }")
    ++ "\n" ++ "}"
    ++ "\n"

  where
    checkTrue f = case f of
      TTrue -> False
      _     -> True

    printSignal sig =
      "\n    " ++ sig ++ ";"

    pr = concatMap ((++ ";") . ("\n    " ++))

-----------------------------------------------------------------------------
