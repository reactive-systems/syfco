-----------------------------------------------------------------------------
-- |
-- Module      :  Writer.Formats.Acacia
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
-- 
-- Transforms a specification to the Ltl2ba / Ltl3ba format.
-- 
-----------------------------------------------------------------------------

module Writer.Formats.Lily where

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

-- | Lily operator configuration.

opConfig
  :: OperatorConfig

opConfig = OperatorConfig
  { tTrue      = "true"
  , fFalse     = "false"
  , opNot      = UnaryOp "!"    3
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

-- | Acacia / Acacia+ writer.

writeFormat
  :: Configuration -> Specification -> Either Error String

writeFormat c s = do
  (as1,is1,gs1) <- eval c s
  as2 <- mapM (simplify (adjust c opConfig) . adjustAtomic) as1
  is2 <- mapM (simplify (adjust c opConfig) . Globally . adjustAtomic) is1
  gs2 <- mapM (simplify (adjust c opConfig) . adjustAtomic) gs1
    
  let
    as3 = map (printFormula opConfig (outputMode c)) as2
    is3 = map (printFormula opConfig (outputMode c)) is2
    gs3 = map (printFormula opConfig (outputMode c)) gs2
  
    as4 = map (\x -> "assume " ++ x ++ ";") as3
    is4 = map (++ ";") is3
    gs4 = map (++ ";") gs3

    xs = case as4 ++ is4 ++ gs4 of
      [] -> []
      ys -> map (++ "\n") (init ys) ++ [last ys]

  return $ concat xs

  where
    adjustAtomic fml = case fml of
      Not (Atomic (Output x)) -> Atomic (Output ("(" ++ x ++ "=0)"))
      Not (Atomic (Input x))  -> Atomic (Input ("(" ++ x ++ "=0)"))
      Atomic (Output x)       -> Atomic (Output ("(" ++ x ++ "=1)"))            
      Atomic (Input x)        -> Atomic (Input ("(" ++ x ++ "=1)"))
      _                       -> applySub adjustAtomic fml

-----------------------------------------------------------------------------



