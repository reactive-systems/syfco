-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Types
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- Types of the different expressions, semantics and targets.
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric #-}

-----------------------------------------------------------------------------

module Data.Types
  ( Target(..)
  , Semantics(..)
  , SignalType(..)
  , IdType(..)
  , SignalDecType(..)
  ) where

-----------------------------------------------------------------------------

import GHC.Generics
  ( Generic
  )

import Generics.Deriving.Enum
  ( GEnum
  )

import Data.Utils
  ( MachinePrintable(..)
  )

import Data.Expression
  ( ExprPos
  , Expr
  )

-----------------------------------------------------------------------------

-- | Target types.

data Target =
    TargetMealy
  | TargetMoore
  deriving (Generic, Eq)

-----------------------------------------------------------------------------

-- | MachinePrintable instance for Target.

instance GEnum Target

instance MachinePrintable Target where
  mprint t = case t of
    TargetMealy -> "mealy"
    TargetMoore -> "moore"

-----------------------------------------------------------------------------

-- | Semantic types.

data Semantics =
    SemanticsMealy
  | SemanticsMoore
  | SemanticsStrictMealy
  | SemanticsStrictMoore
  deriving (Show, Eq, Generic)

-----------------------------------------------------------------------------

-- | MachinePrintable instance for Semantics.

instance GEnum Semantics

instance MachinePrintable Semantics where
  mprint s = case s of
    SemanticsMealy -> "mealy"
    SemanticsMoore -> "moore"
    SemanticsStrictMealy -> "mealy,strict"
    SemanticsStrictMoore -> "moore,strict"

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
