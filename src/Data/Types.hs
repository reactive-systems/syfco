-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Types
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- Types of the different expressions, semantics and targets.
--
-----------------------------------------------------------------------------

{-# LANGUAGE

    LambdaCase

  #-}

-----------------------------------------------------------------------------

module Data.Types
  ( Target(..)
  , Semantics(..)
  , SignalType(..)
  , IdType(..)
  , SignalDecType(..)
  ) where

-----------------------------------------------------------------------------

import Print
  ( Print(..)
  )

import Parse
  ( Parse(..)
  )

import Data.Error
  ( conversionError
  )

import Data.Expression
  ( ExprPos
  , Expr
  )

-----------------------------------------------------------------------------

-- | Target types.

data Target =
    TargetMealy
    -- ^ Mealy machine target
  | TargetMoore
    -- ^ Moore machine target
  deriving (Eq, Ord)

-----------------------------------------------------------------------------

-- | Machine readable instance for Target.

instance Print Target where
  toString = \case
    TargetMealy -> "mealy"
    TargetMoore -> "moore"

-----------------------------------------------------------------------------

-- | Target parser.

instance Parse Target where
  fromString = \case
    "mealy" -> return TargetMealy
    "moore" -> return TargetMoore
    str     -> conversionError
                "Target Parser"
                ("Conversion failed: " ++ str)

-----------------------------------------------------------------------------

-- | Semantic types.

data Semantics =
    SemanticsMealy
    -- ^ Standard Mealy machine semantics.
  | SemanticsMoore
    -- ^ Standard Moore machine semantics.
  | SemanticsStrictMealy
    -- ^ Mealy machine semantics with strict envionment assumptions.
  | SemanticsStrictMoore
    -- ^ Moore machine semantics with strict envionment assumptions.
  deriving (Eq, Ord)

-----------------------------------------------------------------------------

-- | Machine readable instance for Semantics.

instance Print Semantics where
  toString = \case
    SemanticsMealy       -> "mealy"
    SemanticsMoore       -> "moore"
    SemanticsStrictMealy -> "mealy,strict"
    SemanticsStrictMoore -> "moore,strict"

-----------------------------------------------------------------------------

-- | Semantics parser.

instance Parse Semantics where
  fromString = \case
    "mealy"        -> return SemanticsMealy
    "moore"        -> return SemanticsMoore
    "mealy,strict" -> return SemanticsStrictMealy
    "moore,strict" -> return SemanticsStrictMoore
    str            -> conversionError
                       "Semantics Parser"
                       ("Conversion failed: " ++ str)

-----------------------------------------------------------------------------

-- | Signal types.

data SignalType =
    STInput
  | STOutput
  | STGeneric
  deriving (Eq)

-----------------------------------------------------------------------------

-- | Signal decleration types.

data SignalDecType a =
    SDSingle (a,ExprPos)
  | SDBus (a,ExprPos) (Expr a)
  | SDEnum (a,ExprPos) (a,ExprPos)

-----------------------------------------------------------------------------

-- | Expression types.

data IdType =
    TEmptySet
  | TSignal SignalType
  | TBus SignalType
  | TTypedBus SignalType String Int
  | TEnum String Int
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
    TEmptySet               -> "empty set"
    TSignal STInput         -> "input signal"
    TSignal STOutput        -> "output signal"
    TSignal STGeneric       -> "signal"
    TBus STInput            -> "input bus"
    TBus STOutput           -> "output bus"
    TBus STGeneric          -> "bus"
    TTypedBus STInput t _   -> t ++ " input bus"
    TTypedBus STOutput t _  -> t ++ " output bus"
    TTypedBus STGeneric t _ -> t ++ " bus"
    TEnum t _               -> t
    TNumber                 -> "numerical"
    TBoolean                -> "boolean"
    TLtl                    -> "ltl"
    TPattern                -> "pattern"
    TPoly y                 -> "a" ++ show y
    TSet y                  -> show y ++ " set"

-----------------------------------------------------------------------------
