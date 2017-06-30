-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Specification
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- Internal data structure of a specification.
--
-----------------------------------------------------------------------------

module Data.Specification
  ( Specification(..)
  , Expression
  ) where

-----------------------------------------------------------------------------

import Data.Types
  ( SignalDecType
  )

import Data.Expression
  ( Expr
  , ExprPos
  )

import Data.Types
  ( Semantics
  , Target
  )

import Data.Binding
  ( Binding
  )

import Data.SymbolTable
  ( SymbolTable
  )

import Data.Enum
  ( EnumDefinition
  )

-----------------------------------------------------------------------------

-- | We use the type @Expression@ as a shortcut for expressions, where
-- identifiers are denoted by integers.

type Expression = Expr Int

-----------------------------------------------------------------------------

-- | Internal representation of a specification.

data Specification =
  Specification
    { -- | Returns the TSLF source of a specification.
      source :: String

    , -- | Returns the title of a specification.
      title :: String

    , -- | Returns the description of a specification.
      description :: String

    , -- | Returns the semantics of a specification.
      semantics :: Semantics

    , -- | Returns the target flag of a specification.
      target :: Target

    , -- | Returns the tag list of a specification.
      tags :: [String]

    , -- | Positions of the tags in the tags list. Each expression
      -- matches with the corresponding tag in order.
      tagsPos :: [ExprPos]

    , -- | Position of the title in the source file.
      titlePos :: ExprPos

    , -- | Position of the description in the source file.
      descriptionPos :: ExprPos

    , -- | Position of the semantics flag in the source file.
      semanticsPos :: ExprPos

    , -- | Poisition of the target flag in the source file.
      targetPos :: ExprPos

    , -- | List of enumeration definitions.
      enumerations :: [EnumDefinition Int]

    , -- | List of bindings of an identifier to an expression defined in
      -- the PARAMETERS subsection.
      parameters :: [Binding]

    , -- | List of bindings of an identifier to any other expression,
      -- defined in the DEFINITIONS subsection.
      definitions :: [Binding]

    , -- | List of input signals.
      inputs :: [SignalDecType Int]

    , -- | List of output signals.
      outputs :: [SignalDecType Int]

    , -- | List of expresssions representing the initial input of the
      -- environment.
      initially :: [Expression]

    , -- | List of expresssions representing the initial output of the
      -- system.
      preset :: [Expression]

    , -- | List of expresssions representing the globally asserted
      -- requirements on the inputs of the specification.
      requirements :: [Expression]

    , -- | List of expresssions representing the assumptions of the
      -- specification.
      assumptions :: [Expression]

    , -- | List of expressions representing the invariants of the
      -- specification.
      invariants :: [Expression]

    , -- | List of expressions representing the guarantees of the
      -- specification.
      guarantees :: [Expression]

    , -- | Symbol table used to access information about an identifier.
      symboltable :: SymbolTable
    }

-----------------------------------------------------------------------------
