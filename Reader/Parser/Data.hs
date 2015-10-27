module Reader.Parser.Data
       ( Specification(..)
       , globalDef
       ) where

---

import Data.Types
import Data.Binding
import Data.Expression

import Text.Parsec
import Text.Parsec.Token 
import Text.Parsec.Language

---

data Specification =
  Specification
  { title :: String
  , description :: String
  , semantics :: Semantics
  , target :: Target
  , tags :: [String]  
  , parameters :: [Bind Expr String]
  , definitions :: [Bind Expr String]    
  , inputs :: [Bind Expr String]
  , outputs :: [Bind Expr String]    
  , assumptions :: [Expr String]
  , invariants :: [Expr String]
  , guarantees :: [Expr String]
  }

---

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

---

