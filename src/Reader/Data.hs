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
-- module. It includes:
-- 
--     * The title of the specification
-- 
--     * The description of the specification
-- 
--     * The semantics flag of the specification
-- 
--     * The target flag of the specification
-- 
--     * The tag list of the specification
-- 
--     * The list of bindings of an identifier to an
--       expression defined in the PARAMETERS subsection
-- 
--     * The list of bindings of an identifier to any
--       other expression defined in the DEFINITIONS subsection
-- 
--     * The list of input signals
-- 
--     * The list of output signals
-- 
--     * The list of expresssions representing the
--       assumptions of the specification
-- 
--     * The list of expressions representing the
--       invariants of the specification
-- 
--     * The list of expressions representing the
--       guarantees of the specification
-- 
--     * The id to bound expression mapping
-- 
--     * The id to name mapping
-- 
--     * The id to source position mapping
-- 
--     * The id to arguments mapping
-- 
--     * The id to depending ids mapping
-- 
--     * The id to type of the bound expression mapping

data Specification =
  Specification
  { title :: String
  , description :: String
  , semantics :: (Semantics, ExprPos)
  , target :: (Target, ExprPos)
  , tags :: [String]
  , parameters :: [Binding]
  , definitions :: [Binding]    
  , inputs :: [Binding]
  , outputs :: [Binding]    
  , assumptions :: [Expr Int]
  , invariants :: [Expr Int]
  , guarantees :: [Expr Int]
  , bindings :: ExpressionTable
  , names :: NameTable
  , positions :: PositionTable
  , arguments :: ArgumentTable
  , dependencies :: DependencyTable  
  , types :: TypeTable
  }

-----------------------------------------------------------------------------
