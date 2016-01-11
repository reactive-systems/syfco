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
    , applySub  
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

-----------------------------------------------------------------------------

-- | Applies function 'f' to the fist level sup-expressions of 'e'.
  

applySub
  :: (Expr a -> Expr a) -> Expr a -> Expr a

applySub f e =
  let
    e' = case expr e of
      BaseWild         -> BaseWild
      BaseTrue         -> BaseTrue
      BaseFalse        -> BaseFalse
      BaseCon i        -> BaseCon i
      BaseOtherwise    -> BaseOtherwise
      BaseId i         -> BaseId i
      NumSMin x        -> NumSMin $ f x
      NumSMax x        -> NumSMax $ f x
      NumSSize x       -> NumSSize $ f x
      NumSizeOf x      -> NumSizeOf $ f x
      BlnNot x         -> BlnNot $ f x
      LtlNext x        -> LtlNext $ f x
      LtlGlobally x    -> LtlGlobally $ f x
      LtlFinally x     -> LtlFinally  $ f x
      BaseBus x i      -> BaseBus (f x) i          
      NumPlus x y      -> NumPlus (f x) (f y)
      NumMinus x y     -> NumMinus (f x) (f y)
      NumMul x y       -> NumMul (f x) (f y)
      NumDiv x y       -> NumDiv (f x) (f y)
      NumMod x y       -> NumMod (f x) (f y)
      SetCup x y       -> SetCup (f x) (f y)
      SetCap x y       -> SetCap (f x) (f y)
      SetMinus x y     -> SetMinus (f x) (f y)
      BlnEQ x y        -> BlnEQ (f x) (f y)
      BlnNEQ x y       -> BlnNEQ (f x) (f y)
      BlnGE x y        -> BlnGE (f x) (f y)
      BlnGEQ x y       -> BlnGEQ (f x) (f y)
      BlnLE x y        -> BlnLE (f x) (f y)
      BlnLEQ x y       -> BlnLEQ (f x) (f y)
      BlnElem x y      -> BlnElem (f x) (f y)
      BlnOr x y        -> BlnOr (f x) (f y)
      BlnAnd x y       -> BlnAnd (f x) (f y)
      BlnImpl x y      -> BlnImpl (f x) (f y)
      BlnEquiv x y     -> BlnEquiv (f x) (f y)
      LtlRNext x y     -> LtlRNext (f x) (f y)
      LtlRGlobally x y -> LtlRGlobally (f x) (f y)
      LtlRFinally x y  -> LtlRFinally (f x) (f y)
      LtlUntil x y     -> LtlUntil (f x) (f y)
      LtlWeak x y      -> LtlWeak (f x) (f y)
      LtlRelease x y   -> LtlRelease (f x) (f y)
      Colon x y        -> Colon (f x) (f y)
      Pattern x y      -> Pattern (f x) (f y)
      SetRange x y z   -> SetRange (f x) (f y) (f z)
      SetExplicit xs   -> SetExplicit $ map f xs
      BaseFml xs i     -> BaseFml (map f xs) i
      NumRPlus xs x    -> NumRPlus (map f xs) (f x)
      NumRMul xs x     -> NumRMul (map f xs) (f x)
      SetRCup xs x     -> SetRCup (map f xs) (f x)
      SetRCap xs x     -> SetRCap (map f xs) (f x)
      BlnROr xs x      -> BlnROr (map f xs) (f x)
      BlnRAnd xs x     -> BlnRAnd (map f xs) (f x)
  in
    Expr e' $ srcPos e

-----------------------------------------------------------------------------

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
