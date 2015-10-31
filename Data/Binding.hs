-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Binding
-- Description :  A data type to store an identifier bound to an expression
-- License     :  MIT (see the LICENSE file)
-- 
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
-- 
-- A data type to store an identifier bound to an expression
-- 
-----------------------------------------------------------------------------

module Data.Binding
    ( Binding
    , BindExpr(..)
    ) where

-----------------------------------------------------------------------------

import Data.Expression
    ( Expr
    , ExprPos
    )

-----------------------------------------------------------------------------

-- | We use the type @Binding@ as a shortcut for a binding of an expression
-- to an integer.

type Binding = BindExpr Int

-----------------------------------------------------------------------------

-- | The data type @Bind a@ expresses a binding of some instance of type
-- @a@ to some expression. The identifiers inside this expression need to
-- be represented by instances of type @a@ as well. Finally, a binding also
-- containts the source position of the bound identifier as well as possible
-- arguments in case the bindings represents a function.

data BindExpr a =
  BindExpr
  { bIdent :: a
  , bArgs :: [(a,ExprPos)]    
  , bPos :: ExprPos      
  , bVal :: [Expr a]
  } deriving (Show)

-----------------------------------------------------------------------------             

