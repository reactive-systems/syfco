-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Types
-- Description :  Types of the different expressions, semantics and targets
-- License     :  MIT (see the LICENSE file)
-- 
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
-- 
-- Types of the different expressions, semantics and targets
-- 
-----------------------------------------------------------------------------

module Data.Types
    ( Target(..)
    , Semantics(..)
    , SignalType(..)
    , IdType(..)
    ) where

-----------------------------------------------------------------------------

-- | Target types.

data Target =
    TargetMealy
  | TargetMoore

-----------------------------------------------------------------------------

-- | Semantic types.

data Semantics =
    SemanticsMealy
  | SemanticsMoore
  | SemanticsStrictMealy
  | SemanticsStrictMoore

-----------------------------------------------------------------------------

-- | Signal types.

data SignalType =
    STInput
  | STOutput
  | STGeneric
  deriving (Eq)

-----------------------------------------------------------------------------

-- | Expression types.

data IdType =
    TEmptySet
  | TSignal SignalType
  | TNumber
  | TBoolean    
  | TLtl    
  | TPattern    
  | TPoly Int       
  | TSet IdType
  deriving (Eq)

-----------------------------------------------------------------------------

instance Show IdType where
  show x = case x of
    TEmptySet         -> "empty set" 
    TSignal STInput   -> "input signal"
    TSignal STOutput  -> "output signal"
    TSignal STGeneric -> "signal"
    TNumber           -> "numerical"
    TBoolean          -> "boolean"    
    TLtl              -> "ltl"
    TPattern          -> "pattern"
    TPoly y           -> "a" ++ show y    
    TSet y            -> show y ++ " set"

-----------------------------------------------------------------------------
