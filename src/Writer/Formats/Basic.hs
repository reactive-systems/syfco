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
  { tTrue     = "true"
  , fFalse    = "false"
  , opNot      = UnaryOp "!"   1
  , opAnd      = BinaryOp "&&"  2 AssocLeft
  , opOr       = BinaryOp "||"  3 AssocLeft
  , opImplies  = BinaryOp "->"  4 AssocRight
  , opEquiv    = BinaryOp "<->" 4 AssocRight
  , opNext     = UnaryOp  "X"   1 
  , opFinally  = UnaryOp  "F"   1 
  , opGlobally = UnaryOp  "G"   1 
  , opUntil    = BinaryOp "U"   6 AssocRight
  , opRelease  = BinaryOp "R"   7 AssocLeft
  , opWeak     = BinaryOp "W"   5 AssocRight
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

  (as,is,gs) <- eval c s
  as' <- mapM (simplify (adjust c opConfig)) as
  is' <- mapM (simplify (adjust c opConfig)) is
  gs' <- mapM (simplify (adjust c opConfig)) gs  
  return $
    "INFO {"
    ++ "\n" ++ "  TITLE:       \"" ++ title s' ++ "\""
    ++ "\n" ++ "  DESCRIPTION: \"" ++ description s' ++ "\""
    ++ "\n" ++ "  SEMANTICS:   " ++ 
      (case semantics s' of
         SemanticsMealy       -> "Mealy"
         SemanticsMoore       -> "Moore"
         SemanticsStrictMealy -> "Strict,Mealy"
         SemanticsStrictMoore -> "Strict,Moore")    
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
    ++ concatMap printSignal (fmlInputs $
        Implies (And as') (And ((Globally $ And is') : gs')))
    ++ "\n" ++ "  }"
    ++ "\n" ++ "  OUTPUTS {"
    ++ concatMap printSignal (fmlOutputs $
        Implies (And as') (And ((Globally $ And is') : gs')))
    ++ "\n" ++ "  }"
    ++ (if not $ any checkTrue as' then "" 
        else "\n" ++ "  ASSUMPTIONS {" ++
             concatMap pr (filter checkTrue as') ++
             "\n" ++ "  }")
    ++ (if not $ any checkTrue is' then "" 
        else "\n" ++ "  INVARIANTS {" ++
             concatMap pr (filter checkTrue is') ++
             "\n" ++ "  }")
    ++ (if not $ any checkTrue gs' then "" 
        else "\n" ++ "  GUARANTEES {" ++
             concatMap pr (filter checkTrue gs') ++
             "\n" ++ "  }")
    ++ "\n" ++ "}"
    ++ "\n"

  where
    checkTrue f = case f of
      TTrue -> False
      _     -> True

    printSignal sig = 
      "\n    " ++ sig ++ ";"

    pr = (++ ";") . ("\n    " ++) . printFormula opConfig Fully

-----------------------------------------------------------------------------
