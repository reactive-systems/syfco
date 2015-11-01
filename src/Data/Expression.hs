-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Expression
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
-- 
-- Data types to store expressions and some helper functions.
-- 
-----------------------------------------------------------------------------

module Data.Expression
    ( Constant
    , Expr(..)
    , Expr'(..)
    , ExprPos(..)
    , SrcPos(..)
    , subExpressions
    , prExpr
    ) where

-----------------------------------------------------------------------------

-- | A constant is represted by an integer

type Constant = Int

-----------------------------------------------------------------------------

-- | Each expression consists of two parts, the main expression itself, i.e.,
-- the basic term or an operation, and the position of the expression in the
-- source code. The type $a$ specifies the representation of an identifier.

data Expr a =
  Expr
  { expr :: Expr' a
  , srcPos :: ExprPos
  } deriving (Show,Eq)

-----------------------------------------------------------------------------

-- | An expression is either a basic term or the composition of multiple
-- sub-expressions using an operator. To obtain a stepwise refinement of the
-- parsed data, an expression does not need to be type consistent ia the 
-- first place, e.g., techniqually we could add to boolean expressions.
-- Such behaviour is ruled out later during the type analysis.

data Expr' a =
    BaseWild
  | BaseTrue
  | BaseFalse
  | BaseOtherwise      
  | BaseCon Constant    
  | BaseId a
  | BaseBus (Expr a) a
  | BaseFml [Expr a] a

  | NumSMin (Expr a)
  | NumSMax (Expr a)
  | NumSSize (Expr a)
  | NumSizeOf (Expr a)  
  | NumPlus (Expr a) (Expr a)
  | NumRPlus [Expr a] (Expr a)
  | NumMinus (Expr a) (Expr a)
  | NumMul (Expr a) (Expr a)
  | NumRMul [Expr a] (Expr a)    
  | NumDiv (Expr a) (Expr a)
  | NumMod (Expr a) (Expr a)

  | SetExplicit [Expr a]
  | SetRange (Expr a) (Expr a) (Expr a)
  | SetCup (Expr a) (Expr a)
  | SetRCup [Expr a] (Expr a)    
  | SetCap (Expr a) (Expr a)
  | SetRCap [Expr a] (Expr a)    
  | SetMinus (Expr a) (Expr a)    

  | BlnEQ (Expr a) (Expr a)
  | BlnNEQ (Expr a) (Expr a)
  | BlnGE (Expr a) (Expr a)
  | BlnGEQ (Expr a) (Expr a)
  | BlnLE (Expr a) (Expr a)
  | BlnLEQ (Expr a) (Expr a)
  | BlnElem (Expr a) (Expr a)
  | BlnNot (Expr a)
  | BlnOr (Expr a) (Expr a)
  | BlnROr [Expr a] (Expr a)        
  | BlnAnd (Expr a) (Expr a)
  | BlnRAnd [Expr a] (Expr a)    
  | BlnImpl (Expr a) (Expr a)
  | BlnEquiv (Expr a) (Expr a)
    
  | LtlNext (Expr a)
  | LtlRNext (Expr a) (Expr a)  
  | LtlGlobally (Expr a)
  | LtlRGlobally (Expr a) (Expr a)
  | LtlFinally (Expr a)
  | LtlRFinally (Expr a) (Expr a)
  | LtlUntil (Expr a) (Expr a)
  | LtlWeak (Expr a) (Expr a)
  | LtlRelease (Expr a) (Expr a)

  | Colon (Expr a) (Expr a)
  | Pattern (Expr a) (Expr a)
  deriving (Show, Eq)

-----------------------------------------------------------------------------

-- | The position of an expression is denoted by its starting position and
-- ending position in the source code.

data ExprPos =
  ExprPos
  { srcBegin :: SrcPos
  , srcEnd :: SrcPos
  } deriving (Eq, Ord, Show)


-----------------------------------------------------------------------------

-- | A position in the source code is uniquely identified by its line and
-- column.

data SrcPos =
  SrcPos
  { srcLine :: Int
  , srcColumn :: Int
  } deriving (Eq, Ord, Show)

-----------------------------------------------------------------------------

-- | Returns all direct sub-formulas of the given formula, i.e., the formulas
-- that appear under the first operator. If the given formula is a basic
-- term, an empty list is returned.

subExpressions
  :: Expr a -> [Expr a]

subExpressions e = case expr e of
  BaseWild         -> []
  BaseTrue         -> []
  BaseFalse        -> []
  BaseCon _        -> []
  BaseId _         -> []
  BaseOtherwise    -> []
  BaseBus x _      -> [x]
  NumSMin x        -> [x]
  NumSMax x        -> [x]
  NumSSize x       -> [x]
  NumSizeOf x      -> [x]
  BlnNot x         -> [x]
  LtlNext x        -> [x]
  LtlGlobally x    -> [x]  
  LtlFinally x     -> [x]  
  NumPlus x y      -> [x,y]
  NumMinus x y     -> [x,y]
  NumMul x y       -> [x,y]
  NumDiv x y       -> [x,y]
  NumMod x y       -> [x,y]
  SetCup x y       -> [x,y]
  SetCap x y       -> [x,y]
  SetMinus x y     -> [x,y]
  BlnEQ x y        -> [x,y]
  BlnNEQ x y       -> [x,y]
  BlnGE x y        -> [x,y]
  BlnGEQ x y       -> [x,y]
  BlnLE x y        -> [x,y]
  BlnLEQ x y       -> [x,y]
  BlnElem x y      -> [x,y]
  BlnOr x y        -> [x,y]
  BlnAnd x y       -> [x,y]
  BlnImpl x y      -> [x,y]
  BlnEquiv x y     -> [x,y]
  LtlRNext x y     -> [x,y]  
  LtlRGlobally x y -> [x,y]
  LtlRFinally x y  -> [x,y]
  LtlUntil x y     -> [x,y]
  LtlWeak x y      -> [x,y]
  LtlRelease x y   -> [x,y]
  Colon x y        -> [x,y]
  Pattern x y      -> [x,y]
  SetRange x y z   -> [x,y,z]
  SetExplicit xs   -> xs
  BaseFml xs _     -> xs    
  NumRPlus xs x    -> x:xs
  NumRMul xs x     -> x:xs  
  SetRCup xs x     -> x:xs  
  SetRCap xs x     -> x:xs    
  BlnROr xs x      -> x:xs
  BlnRAnd xs x     -> x:xs

---

-- | Some debugging function to give a more readable version of the expression.
-- In constrast to @show@, this function drops all position information in the
-- resulting output (for debugging purposes only).

prExpr
  :: Expr Int -> String

prExpr e = case expr e of
  BaseWild         -> "WILD"
  BaseTrue         -> "TRUE"
  BaseFalse        -> "FALSE"
  BaseOtherwise    -> "OTHERWISE"
  BaseCon x        -> "(CON " ++ show x ++ ")"
  BaseId x         -> "(ID " ++ show x ++ ")"
  BaseBus x y      -> "(BUS " ++ show y ++ "[" ++ prExpr x ++ "])"
  BaseFml xs y     -> "(FUN " ++ show y ++ "(" ++
                      (if null xs then "" else (prExpr $ head xs) ++
                                               (concatMap (((:) ',') . prExpr)
                                                $ tail xs)) ++ ")"
  NumSMin x        -> "(MIN " ++ prExpr x ++ ")"
  NumSMax x        -> "(MAX " ++ prExpr x ++ ")"
  NumSSize x       -> "(SIZE " ++ prExpr x ++ ")"
  NumSizeOf x      -> "(SIZEOF " ++ prExpr x ++ ")"  
  BlnNot x         -> "(NOT " ++ prExpr x ++ ")"
  LtlNext x        -> "(X " ++ prExpr x ++ ")"
  LtlGlobally x    -> "(G " ++ prExpr x ++ ")"
  LtlFinally x     -> "(F " ++ prExpr x ++ ")"
  NumPlus x y      -> "(PLUS " ++ prExpr x ++ " " ++ prExpr y ++ ")"
  NumMinus x y     -> "(MINUS " ++ prExpr x ++ " " ++ prExpr y ++ ")"
  NumMul x y       -> "(MUL " ++ prExpr x ++ " " ++ prExpr y ++ ")"
  NumDiv x y       -> "(DIV " ++ prExpr x ++ " " ++ prExpr y ++ ")"
  NumMod x y       -> "(MOD " ++ prExpr x ++ " " ++ prExpr y ++ ")"
  SetCup x y       -> "(CUP " ++ prExpr x ++ " " ++ prExpr y ++ ")"
  SetCap x y       -> "(CAP " ++ prExpr x ++ " " ++ prExpr y ++ ")"
  SetMinus x y     -> "(DIFF " ++ prExpr x ++ " " ++ prExpr y ++ ")"
  BlnEQ x y        -> "(EQ " ++ prExpr x ++ " " ++ prExpr y ++ ")"
  BlnNEQ x y       -> "(NEQ " ++ prExpr x ++ " " ++ prExpr y ++ ")"
  BlnGE x y        -> "(GE " ++ prExpr x ++ " " ++ prExpr y ++ ")"
  BlnGEQ x y       -> "(GEQ " ++ prExpr x ++ " " ++ prExpr y ++ ")"
  BlnLE x y        -> "(LE " ++ prExpr x ++ " " ++ prExpr y ++ ")"
  BlnLEQ x y       -> "(LEQ " ++ prExpr x ++ " " ++ prExpr y ++ ")"
  BlnElem x y      -> "(ELEM " ++ prExpr x ++ " " ++ prExpr y ++ ")"
  BlnOr x y        -> "(OR " ++ prExpr x ++ " " ++ prExpr y ++ ")"
  BlnAnd x y       -> "(AND " ++ prExpr x ++ " " ++ prExpr y ++ ")"
  BlnImpl x y      -> "(IMPL " ++ prExpr x ++ " " ++ prExpr y ++ ")"
  BlnEquiv x y     -> "(EQIV " ++ prExpr x ++ " " ++ prExpr y ++ ")"
  LtlRNext x y     -> "(X[" ++ prExpr x ++ "] " ++ prExpr y ++ ")"
  LtlRGlobally x y -> "(G[" ++ prExpr x ++ "] " ++ prExpr y ++ ")"
  LtlRFinally x y  -> "(F[" ++ prExpr x ++ "] " ++ prExpr y ++ ")"
  LtlUntil x y     -> "(U " ++ prExpr x ++ " " ++ prExpr y ++ ")"
  LtlWeak x y      -> "(W " ++ prExpr x ++ " " ++ prExpr y ++ ")"
  LtlRelease x y   -> "(R " ++ prExpr x ++ " " ++ prExpr y ++ ")"
  Colon x y        -> prExpr x ++ " : " ++ prExpr y 
  Pattern x y      -> prExpr x ++ " ~ " ++ prExpr y 
  SetRange x y z   -> "(SR " ++ prExpr x ++ " " ++ prExpr y ++ " " ++ prExpr z ++ ")"
  SetExplicit xs   -> "(SET " ++ concatMap (((flip (++)) " ") . prExpr) xs ++ ")"
  NumRPlus xs x    -> "(PLUS[" ++ concatMap (((flip (++)) " ") . prExpr) xs ++ "] " ++ prExpr x ++ ")"
  NumRMul xs x     -> "(MUL[" ++ concatMap (((flip (++)) " ") . prExpr) xs ++ "] " ++ prExpr x ++ ")"
  SetRCup xs x     -> "(CUP[" ++ concatMap (((flip (++)) " ") . prExpr) xs ++ "] " ++ prExpr x ++ ")"
  SetRCap xs x     -> "(CAP[" ++ concatMap (((flip (++)) " ") . prExpr) xs ++ "] " ++ prExpr x ++ ")"
  BlnROr xs x      -> "(OR[" ++ concatMap (((flip (++)) " ") . prExpr) xs ++ "] " ++ prExpr x ++ ")"
  BlnRAnd xs x     -> "(AND[" ++ concatMap (((flip (++)) " ") . prExpr) xs ++ "] " ++ prExpr x ++ ")"

---
