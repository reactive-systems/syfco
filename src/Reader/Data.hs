-----------------------------------------------------------------------------
-- |
-- Module      :  Reader.Data
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- Common data used by the reader module.
--
-----------------------------------------------------------------------------

module Reader.Data
  ( NameTable
  , PositionTable
  , ArgumentTable
  , ExpressionTable
  , TypeTable
  , DependencyTable
  , Specification(..)
  ) where

-----------------------------------------------------------------------------

import Data.Types
  ( SignalDecType
  )

import Data.Enum
  ( EnumDefinition
  )

import Data.Types
  ( IdType
  , Semantics
  , Target
  )

import Data.Expression
  ( Expr
  , ExprPos
  )

import Data.Binding
  ( Binding
  )

import  Data.IntMap.Strict
  ( IntMap
  )

-----------------------------------------------------------------------------

-- | Mapping to map identifier IDs to their correpsonding name.

type NameTable = IntMap String

-----------------------------------------------------------------------------

-- | Mapping to map identifier IDs to their defining position in the source
-- file.

type PositionTable = IntMap ExprPos

-----------------------------------------------------------------------------

-- | Mapping to map identifier IDs to their arguments, in case the bound
-- expression is a function.

type ArgumentTable   = IntMap [Int]

-----------------------------------------------------------------------------

-- | Mapping to map identifier IDs to their bound expression.

type ExpressionTable = IntMap (Expr Int)

-----------------------------------------------------------------------------

-- | Mapping to map identifier IDs to their correspoinging type.

type TypeTable       = IntMap IdType

-----------------------------------------------------------------------------

-- | Mapping to map identifier IDs to their the list of identifier IDs it
-- depends on.

type DependencyTable = IntMap [Int]

-----------------------------------------------------------------------------

-- | The internal representation of a specification used by the reader
-- module.

data Specification =
  Specification
  { -- | The title of the specification.
    title :: (String, ExprPos)

  , -- | The description of the specification.
   description :: (String, ExprPos)

  , -- | The semantics flag of the specification.
    semantics :: (Semantics, ExprPos)

  , -- | The target flag of the specification.
    target :: (Target, ExprPos)

  , -- | The tag list of the specification.
    tags :: [(String, ExprPos)]

  , -- | The list of defined enumeration types.
    enumerations :: [EnumDefinition Int]

  , -- | The list of bindings of an identifier to an expression
    -- defined in the PARAMETERS subsection.
    parameters :: [Binding]

  , -- | The list of bindings of an identifier to any other expression
    -- defined in the DEFINITIONS subsection.
    definitions :: [Binding]

  , -- | The list of input signals.
    inputs :: [SignalDecType Int]

  , -- | The list of output signals.
    outputs :: [SignalDecType Int]

  , -- | The list of expresssions representing the initial input of
    -- the environment.
    initially :: [Expr Int]

  , -- | The list of expresssions representing the initial output of
    -- the system.
    preset :: [Expr Int]

  , -- | The list of expresssions representing the globally asserted
    -- requirements on the inputs of the specification.
    requirements :: [Expr Int]

  , -- | The list of expresssions representing the assumptions of the
    -- specification.
    assumptions :: [Expr Int]

  , -- | The list of expressions representing the invariants of the
    -- specification.
    invariants :: [Expr Int]

  , -- | The list of expressions representing the guarantees of the
    -- specification.
    guarantees :: [Expr Int]

  , -- | The id to bounded-expression mapping.
    bindings :: ExpressionTable

  , -- | The id to name mapping.
    names :: NameTable

  , -- | The id to source position mapping.
    positions :: PositionTable

  , -- | The id to arguments mapping.
    arguments :: ArgumentTable

  , -- | The id to depending ids mapping.
    dependencies :: DependencyTable

  , -- | The id to type of the bound expression mapping.
    types :: TypeTable

  }

-----------------------------------------------------------------------------
