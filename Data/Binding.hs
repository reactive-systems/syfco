module Data.Binding where

---

import Data.Expression

---

type Binding = Bind Expr Int

---

data Bind a b =
  Bind
  { bIdent :: b
  , bArgs :: [(b,ExprPos)]    
  , bPos :: ExprPos      
  , bVal :: [a b]
  } deriving (Show)

---             
