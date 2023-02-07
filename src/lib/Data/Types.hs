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
  , MultiParamTypeClasses
  , TypeSynonymInstances
  , FlexibleInstances

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

import Data.Convertible
  ( Convertible(..)
  , ConvertError(..)
  )

import Data.Expression
  ( ExprPos
  , Expr
  )

import Data.Char
  ( toLower
  )

import Control.Arrow
  ( (>>>)
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

instance Convertible Target String where
  safeConvert = return . \case
    TargetMealy -> "mealy"
    TargetMoore -> "moore"

-----------------------------------------------------------------------------

instance Convertible String Target where
  safeConvert = map toLower >>> \case
    "mealy" -> return TargetMealy
    "moore" -> return TargetMoore
    str     -> Left ConvertError
      { convSourceValue = str
      , convSourceType = "String"
      , convDestType = "Target"
      , convErrorMessage = "Unknown target"
      }

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
  | SemanticsFiniteMealy
    -- ^ Mealy machine semantics with finite-trace assumptions.
  | SemanticsFiniteMoore
    -- ^ Moore machine semantics with finite-trace assumptions.
  deriving (Eq, Ord)

-----------------------------------------------------------------------------

instance Convertible Semantics String where
  safeConvert = return . \case
    SemanticsMealy       -> "mealy"
    SemanticsMoore       -> "moore"
    SemanticsStrictMealy -> "mealy,strict"
    SemanticsStrictMoore -> "moore,strict"
    SemanticsFiniteMealy -> "mealy,finite"
    SemanticsFiniteMoore -> "moore,finite"

-----------------------------------------------------------------------------

instance Convertible String Semantics where
  safeConvert = map toLower >>> \case
    "mealy"        -> return SemanticsMealy
    "moore"        -> return SemanticsMoore
    "mealy,strict" -> return SemanticsStrictMealy
    "moore,strict" -> return SemanticsStrictMoore
    "mealy,finite" -> return SemanticsFiniteMealy
    "moore,finite" -> return SemanticsFiniteMoore
    str            -> Left ConvertError
      { convSourceValue = str
      , convSourceType = "String"
      , convDestType = "Semantics"
      , convErrorMessage = "Unknown semantics"
      }

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
