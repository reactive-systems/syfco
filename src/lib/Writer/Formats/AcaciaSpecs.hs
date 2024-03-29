-----------------------------------------------------------------------------
-- |
-- Module      :  Writer.Formats.AcaciaSpecs
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Guillermo Perez (gperezme@ulb.ac.be)
--                Felix Klein (klein@react.uni-saarland.de)
--
-- Transforms a specification to the Acacia+ format including a unit
-- separation.
--
-----------------------------------------------------------------------------

module Writer.Formats.AcaciaSpecs where

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

-- | Acacia / Acacia+ operator configuration.

opConfig
  :: OperatorConfig

opConfig = OperatorConfig
  { tTrue          = "true"
  , fFalse         = "false"
  , opNot          = UnaryOp "!"    1
  , opAnd          = BinaryOp "*"   3 AssocLeft
  , opOr           = BinaryOp "+"   3 AssocLeft
  , opImplies      = BinaryOp "->"  3 AssocLeft
  , opEquiv        = BinaryOp "<->" 3 AssocLeft
  , opNext         = UnaryOp  "X"   1
  , opStrongNext   = UnaryOpUnsupported
  , opWeakNext     = UnaryOpUnsupported
  , opPrevious     = UnaryOpUnsupported
  , opFinally      = UnaryOp  "F"   1
  , opGlobally     = UnaryOp  "G"   1
  , opHistorically = UnaryOpUnsupported
  , opOnce         = UnaryOpUnsupported
  , opUntil        = BinaryOp "U"   2 AssocLeft
  , opRelease      = BinaryOpUnsupported
  , opWeak         = BinaryOpUnsupported
  , opSince        = BinaryOpUnsupported
  , opTriggered    = BinaryOpUnsupported
  }

-----------------------------------------------------------------------------

-- | Acacia / Acacia+ writer.

writeFormat
  :: Configuration -> Specification -> Either Error String

writeFormat c s = do
  (es1,ss1,rs1,as1,is1,gs1) <- eval c s

  as2 <- mapM (simplify (adjust c opConfig) . adjustAtomic) $
         case ss1 of
           [] -> filter (/= FFalse) $ es1 ++ map fGlobally rs1 ++ as1
           _  -> filter (/= FFalse) $ es1 ++
                  map (\f -> fOr [fNot $ fAnd ss1, f])
                    (map fGlobally rs1 ++ as1)

  is2 <- mapM (simplify (adjust c opConfig) . fGlobally . adjustAtomic) is1
  gs2 <- mapM (simplify (adjust c opConfig) . adjustAtomic) (gs1 ++ ss1)

  as3 <- mapM (printFormula opConfig (outputMode c) (quoteMode c)) as2
  is3 <- mapM (printFormula opConfig (outputMode c) (quoteMode c)) is2
  gs3 <- mapM (printFormula opConfig (outputMode c) (quoteMode c)) gs2

  let
    as4 = map (\x -> "assume " ++ x ++ ";") as3
    is4 = map (++ ";") is3
    gs4 = map (++ ";") gs3

    ws = map (++ "\n") as4
    flatws = concat ws

    xs = case is4 ++ gs4 of
      [] -> []
      ys -> map (++ "\n") (init ys) ++ [last ys]

    zs = zip [(0 :: Int)..] xs

    finals = map (\x -> "[spec_unit " ++ show(fst x) ++ "]\n" ++ flatws ++ (snd x)) zs

  return $ concat finals

  where
    adjustAtomic fml = case fml of
      Not (Atomic (Output x)) -> Atomic (Output ("(" ++ x ++ "=0)"))
      Not (Atomic (Input x))  -> Atomic (Input ("(" ++ x ++ "=0)"))
      Atomic (Output x)       -> Atomic (Output ("(" ++ x ++ "=1)"))
      Atomic (Input x)        -> Atomic (Input ("(" ++ x ++ "=1)"))
      _                       -> applySub adjustAtomic fml

-----------------------------------------------------------------------------
