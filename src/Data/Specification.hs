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

-----------------------------------------------------------------------------

-- | We use the type @Expression@ as a shortcut for expressions, where
-- identifiers are denoted by integers.

type Expression = Expr Int

-----------------------------------------------------------------------------

-- | The internal representation of a specification. It includes:
-- 
--     * The source specification file 
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
--     * The symbol table used to access information about an identifier

data Specification =
  Specification
  { source :: String
  , title :: String
  , description :: String
  , semantics :: Semantics
  , semanticsP :: ExprPos 
  , target :: Target
  , targetP :: ExprPos 
  , tags :: [String]
  , parameters :: [Binding]
  , definitions :: [Binding]    
  , inputs :: [Binding]
  , outputs :: [Binding]    
  , assumptions :: [Expression]
  , invariants :: [Expression]
  , guarantees :: [Expression]
  , symboltable :: SymbolTable
  }

-----------------------------------------------------------------------------  

