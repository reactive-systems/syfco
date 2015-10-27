module Data.Types
       ( Target(..)
       , Semantics(..)
       , SignalType(..)
       , IdType(..)
       ) where

---

data Target =
    TargetMealy
  | TargetMoore

---

data Semantics =
    SemanticsMealy
  | SemanticsMoore
  | SemanticsStrictMealy
  | SemanticsStrictMoore

---

data SignalType =
    STInput
  | STOutput
  | STGeneric
  deriving (Eq)

---

data IdType =
    TPoly Int   
  | TNumber
  | TSignal SignalType
  | TLtl    
  | TBoolean
  | TPattern
  | TEmptySet  
  | TSet IdType
  deriving (Eq)

---

instance Show IdType where
  show x = case x of
    TPoly y -> "a" ++ show y
    TEmptySet -> "empty set" 
    TSignal STInput -> "input signal"
    TSignal STOutput -> "output signal"
    TSignal STGeneric -> "signal"
    TNumber -> "numerical"
    TLtl -> "ltl"
    TBoolean -> "boolean"
    TPattern -> "pattern"
    TSet y -> show y ++ " set"

---
