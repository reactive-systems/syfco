module Data.Specification where

---

import Data.Types
import Data.Binding
import Data.LookupTable
import Data.Expression

---

type Expression = Expr Int

---

data Specification =
  Specification
  { title :: String
  , description :: String
  , semantics :: Semantics
  , target :: Target
  , tags :: [String]
  , parameters :: [Binding]
  , definitions :: [Binding]    
  , inputs :: [Binding]
  , outputs :: [Binding]    
  , assumptions :: [Expression]
  , invariants :: [Expression]
  , guarantees :: [Expression]
  , lookuptable :: LookupTable
  }

---

