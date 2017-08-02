-----------------------------------------------------------------------------
-- |
-- Module      :  Writer.Formats.Rabinizer
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Ayrat Khalimov (ayrat.khalimov@iaik.tugraz.at)
--                Felix Klein (klein@react.uni-saarland.de)
--
-- Transforms a specification to the Rabinizer format.
--
-----------------------------------------------------------------------------

module Writer.Formats.Rabinizer where

-----------------------------------------------------------------------------

import Config
import Simplify

import Data.List
import Data.Char
import Data.LTL
import Data.Error
import Data.Specification

import Control.Monad

import Writer.Eval
import Writer.Data
import Writer.Utils

-----------------------------------------------------------------------------

-- | Rabinizer operator configuration.

opConfig
  :: OperatorConfig

opConfig = OperatorConfig
  { tTrue      = "true"
  , fFalse     = "false"
  , opNot      = UnaryOp  "!"           4
  , opAnd      = BinaryOp "&"           6 AssocLeft
  , opOr       = BinaryOp "|"           7 AssocLeft
  , opImplies  = BinaryOpUnsupported
  , opEquiv    = BinaryOpUnsupported
  , opNext     = UnaryOp  "X"           1
  , opFinally  = UnaryOp  "F"           2
  , opGlobally = UnaryOp  "G"           3
  , opUntil    = BinaryOp "U"           5 AssocLeft
  , opRelease  = BinaryOpUnsupported
  , opWeak     = BinaryOpUnsupported
  }

-----------------------------------------------------------------------------

writeFormat
  :: Configuration -> Specification -> Either Error String

writeFormat c s = do
  (iv,ov) <- signals c s
  let symbols =  concat $ iv ++ ov

  case find (== '\'') symbols of
    Just _  -> when (any (not . isAlphaNum) $ primeSymbol c) $ cfgError $
      "The specification contains primes, which cannot be used " ++
      "inside Rabinizer signal names. Rabinizer only supports " ++
      "alphanumeric signal names.\nThey can be replaced by changing " ++
      "the default value of the \"-ps\" option."
    Nothing -> return ()

  case find (== '@') symbols of
    Just _  -> when (any (not . isAlphaNum) $ atSymbol c) $ cfgError $
      "The specification contains @-symbols, which cannot be used " ++
      "inside Rabinizer signal names. Rabinizer only supports " ++
      "alphanumeric signal names.\nThey can be replaced by changing " ++
      "the default value of the \"-as\" option."
    Nothing -> return ()

  (iv',ov') <- signals c { busDelimiter = "" } s
  case find (== '_') $ concat $ iv' ++ ov' of
    Just _  -> cfgError $
      "The specification contains '_'-symbols, which cannot be used " ++
      "inside Rabinizer signal names. Rabinizer only supports " ++
      "alphanumeric signal names."
    Nothing -> return ()

  case find (not . isAlphaNum) symbols of
    Just _  -> when (all (not . isAlphaNum) $ busDelimiter c) $ cfgError $
      "The specification contains '_'-symbols to denote bus delimiters,\n" ++
      "which cannot be used inside Rabinizer signal names. Rabinizer\n" ++
      "only supports  alphanumeric signal names. They can be replaced\n" ++
      "by changing the default value of the \"-bd\" option."
    Nothing -> return ()

  (es,ss,rs,as,is,gs) <- eval c s
  fml0 <- merge es ss rs as is gs
  fml1 <- simplify (adjust c opConfig) $ noBooleanDerived fml0

  return $ printFormula opConfig (outputMode c) fml1

  where
    noBooleanDerived fml = case fml of
      Implies x y -> Or $ map noBooleanDerived [Not x, y]
      Equiv x y   -> Or [ And $ map noBooleanDerived [x,y]
                       , And $ map noBooleanDerived [Not x, Not y]
                       ]
      _           -> applySub noBooleanDerived fml

-----------------------------------------------------------------------------
