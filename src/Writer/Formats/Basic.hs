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

module Writer.Formats.Basic
    ( writeBasic
    ) where

-----------------------------------------------------------------------------

import Config
import Simplify

import Data.LTL
import Data.Types
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
  , opFinally = "F"
  , opGlobally = "G" 
  , opUntil = "U" 
  , opRelease = "R" 
  , opWeak = "W"
  }

-----------------------------------------------------------------------------

-- | Basic TLSF writer.

writeBasic
  :: Configuration -> Specification -> Either Error String

writeBasic c s = do
  (as,is,gs) <- eval c s
  as' <- mapM (simplify c) as
  is' <- mapM (simplify c) is
  gs' <- mapM (simplify c) gs  
  return $
    "INFO {"
    ++ "\n" ++ "  TITLE:       \"" ++ title s ++ "\""
    ++ "\n" ++ "  DESCRIPTION: \"" ++ description s ++ "\""
    ++ "\n" ++ "  SEMANTICS:   " ++ (case semantics s of
                                        SemanticsMealy       -> "Mealy"
                                        SemanticsMoore       -> "Moore"
                                        SemanticsStrictMealy -> "Strict,Mealy"
                                        SemanticsStrictMoore -> "Strict,Moore")    
    ++ "\n" ++ "  TARGET:      " ++ (case target s of
                                        TargetMealy -> "Mealy"
                                        TargetMoore -> "Moore")
    ++ (if null $ tags s then "" 
        else "\n  TAGS:        " ++ head (tags s) ++
             concatMap ((:) ' ' . (:) ',') (tail $ tags s))
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
             concatMap (pretty mode opNames) (filter checkTrue as') ++
             "\n" ++ "  }")
    ++ (if not $ any checkTrue is' then "" 
        else "\n" ++ "  INVARIANTS {" ++
             concatMap (pretty mode opNames) (filter checkTrue is') ++
             "\n" ++ "  }")
    ++ (if not $ any checkTrue gs' then "" 
        else "\n" ++ "  GUARANTEES {" ++
             concatMap (pretty mode opNames) (filter checkTrue gs') ++
             "\n" ++ "  }")
    ++ "\n" ++ "}"
    ++ "\n"

  where
    mode = outputMode c
    
    checkTrue f = case f of
      TTrue -> False
      _     -> True

    printSignal sig = 
      "\n    " ++ sig ++ ";"

-----------------------------------------------------------------------------
