-----------------------------------------------------------------------------
-- |
-- Module      :  Reader.Parser.Data
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
-- 
-- Common data used by the parser module.
-- 
-----------------------------------------------------------------------------

module Reader.Parser.Data
    ( Specification(..)
    , globalDef
    ) where

-----------------------------------------------------------------------------

import Data.Types
    ( Target
    , Semantics
    )
    
import Data.Binding
    ( BindExpr
    )
    
import Data.Expression
    ( Expr
    )   

import Text.Parsec
    ( (<|>)
    , char  
    , letter
    , alphaNum  
    )
    
import Text.Parsec.Token
    ( LanguageDef
    , GenLanguageDef(..)  
    )
    
import Text.Parsec.Language
    ( emptyDef
    )

-----------------------------------------------------------------------------

-- | The @Specification@ record contains all the data of a 
-- specification that is extracted by the parsing process. This includes:
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

data Specification =
  Specification
  { title :: String
  , description :: String
  , semantics :: Semantics
  , target :: Target
  , tags :: [String]  
  , parameters :: [BindExpr String]
  , definitions :: [BindExpr String]    
  , inputs :: [BindExpr String]
  , outputs :: [BindExpr String]    
  , assumptions :: [Expr String]
  , invariants :: [Expr String]
  , guarantees :: [Expr String]
  }

-----------------------------------------------------------------------------

-- | The language definition which is shared among all parsers.

globalDef
  :: LanguageDef a 

globalDef =
  emptyDef
  { identStart     = letter <|> char '_' <|> char '@' 
  , identLetter    = alphaNum <|> char '_' <|> char '@' <|> char '\''
  , commentLine    = "//"
  , commentStart   = "/*"
  , commentEnd     = "*/"
  , nestedComments = True                       
  , caseSensitive  = True
  }

-----------------------------------------------------------------------------

