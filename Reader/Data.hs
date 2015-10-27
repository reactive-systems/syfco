module Reader.Data where

---

import Data.Types
import Data.Expression
import Data.Binding

import qualified Data.IntMap.Strict as IM

---           

type NameTable       = IM.IntMap String     
type PositionTable   = IM.IntMap ExprPos    
type ArgumentTable   = IM.IntMap [Int]      
type ExpressionTable = IM.IntMap (Expr Int) 
type TypeTable       = IM.IntMap IdType     
type DependencyTable = IM.IntMap [Int]

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

---           
