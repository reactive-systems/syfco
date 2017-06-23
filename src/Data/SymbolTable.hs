-----------------------------------------------------------------------------
-- |
-- Module      :  Data.SymbolTable
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- Data type to store all identifier specific content.
--
-----------------------------------------------------------------------------

{-# LANGUAGE

    ViewPatterns
  , LambdaCase
  , RecordWildCards

  #-}

-----------------------------------------------------------------------------

module Data.SymbolTable
    ( SymbolTable
    , IdRec(..)
    , stToCSV
    ) where

-----------------------------------------------------------------------------

import Data.Types
   ( IdType(..)
   , SignalType(..)
   )

import Data.Expression
   ( Expr(..)
   , Expr'(..)
   , ExprPos(..)
   , SrcPos(..)
   , prExpr
   , expr
   )

import Data.Char
   ( ord
   , chr
   )

import Data.Array
   ( Array
   , (!)
   , assocs
   )

-----------------------------------------------------------------------------

-- | A symbol table is an array mapping identifieres, represend by integers,
-- to blocks of information.

type SymbolTable = Array Int IdRec

-----------------------------------------------------------------------------

data IdRec =

  IdRec
    { -- | The name of the identifier.
      idName :: String

    , -- | The position of the identifer definition in the source file.
      idPos :: ExprPos

    , -- | The arguemnts, in case the identifier describes a function.
      idArgs :: [Int]

    , -- | The expression, the identifier is bound to.
      idBindings :: Expr Int

    , -- | The type of the identifier.
      idType :: IdType

    , -- | The list of identifiers, which have to be evaluated first
      -- to evaluate this identifier.
      idDeps :: [Int]

    }

-----------------------------------------------------------------------------

-- | Prints a symbol table in the CVS format (for debugging purposes only).

stToCSV
  :: SymbolTable -> IO ()

stToCSV lt = do
    putStrLn "Id;Name;Position;Arguments;Bindings;Type;Dependencies;"
    mapM_ printEntry $ assocs lt

  where
    printEntry (i,r@IdRec{..}) = do
      putStr $ show i
      putStr ";"
      putStr ("\"" ++ idName ++ "\"")
      putStr ";"
      putStr $ prExprPos idPos
      putStr ";"
      putStr $ commasepxs idArgs
      putStr ";"
      putStr $ prPrettyExpr lt r idBindings
      putStr ";"
      putStr $ prType idArgs idType
      putStr ";"
      putStr $ commasepxs idDeps
      putStrLn ";"

    commasepxs = \case
      (x:xr) -> show x ++ concatMap ((:) ',') (map show xr)
      []     -> ""

    prExprPos pos =
      let
        bl = srcLine $ srcBegin pos
        bc = srcColumn $ srcBegin pos
        el = srcLine $ srcEnd pos
        ec = srcColumn $ srcEnd pos
      in
        "(" ++ show bl ++ "," ++ show bc ++
        if bl == el then
          "-" ++ show ec ++ ")"
        else
          show el ++ ":" ++ show ec ++ ")"

    prType xs t = concatMap prArgType xs ++ case t of
      TNumber   -> "Int"
      TLtl      -> "Ltl"
      TBoolean  -> "Bool"
      TPattern  -> "Pattern"
      TEmptySet -> "Empty Set"
      TSet x    -> prType [] x ++ " Set"
      TPoly i   ->
        if i >= ord 'a' && i <= ord 'z'
        then [chr i]
        else "a" ++ show i
      TSignal STInput  -> "Input"
      TSignal STOutput  -> "Output"
      TSignal STGeneric -> "In|Out"
      TBus STInput      -> "Input Bus"
      TBus STOutput     -> "Output Bus"
      TBus STGeneric    -> "In|Out Bus"
      TTypedBus STInput s _ -> "Input[" ++ s ++ "]"
      TTypedBus STOutput s _ -> "Output[" ++ s ++ "]"
      TTypedBus STGeneric s _ -> "In|Out[" ++ s ++ "]"
      TEnum s _ -> s

    prArgType x =
      let
        r = lt ! x
        args = idArgs r
      in
         if length args > 1
         then "(" ++ prType args (idType r)  ++ ") -> "
         else prType args (idType r) ++ " -> "

-----------------------------------------------------------------------------

prPrettyExpr
  :: SymbolTable -> IdRec -> Expr Int -> String

prPrettyExpr _  _ (expr -> SetExplicit []) = ""
prPrettyExpr st r e = pr e

  where
    pr = pr' . expr

    pr' = \case
      BaseWild         -> "_"
      BaseTrue         -> "\"true\""
      BaseFalse        -> "\"false\""
      BaseOtherwise    -> "otherwise"
      BaseCon x        -> "\"" ++ show x ++ "\""
      BaseId x         -> "\"" ++ idName (st ! x) ++ "\"<" ++ show x ++ ">"
      BaseBus x y      -> "\"" ++ idName (st ! y) ++ "\"<" ++ show y ++ ">[" ++ pr x ++ "]"
      BaseFml xs y     -> "\"" ++ idName (st ! y) ++ "\"<" ++ show y ++ ">(" ++
                          (if null xs then ""
                           else pr (head xs) ++
                                concatMap ((:) ',' . pr) (tail xs)) ++ ")"
      NumSMin x        -> "min " ++ pr x
      NumSMax x        -> "max " ++ pr x
      NumSSize x       -> "|" ++ pr x ++ "|"
      NumSizeOf x      -> "sizeof " ++ pr x ++ ")"
      BlnNot x         -> "¬" ++ pr x
      LtlNext x        -> "X " ++ pr x
      LtlGlobally x    -> "G " ++ pr x
      LtlFinally x     -> "F " ++ pr x
      NumPlus x y      -> pr x ++ " + " ++ pr y
      NumMinus x y     -> pr x ++ " - " ++ pr y
      NumMul x y       -> pr x ++ " * " ++ pr y
      NumDiv x y       -> pr x ++ " / " ++ pr y
      NumMod x y       -> pr x ++ " % " ++ pr y
      SetCup (expr -> SetExplicit []) x -> pr x
      SetCup x (expr -> SetExplicit []) -> pr x
      SetCup x y                       -> pr x ++ " ∪ " ++ pr y
      SetCap (expr -> SetExplicit []) x -> pr x
      SetCap x (expr -> SetExplicit []) -> pr x
      SetCap x y                       -> pr x ++ " ∩ " ++ pr y
      SetMinus x y     -> pr x ++ " ∖ " ++ pr y
      BlnEQ x y        -> pr x ++ " = " ++ pr y
      BlnNEQ x y       -> pr x ++ " ≠ " ++ pr y
      BlnGE x y        -> pr x ++ " > " ++ pr y
      BlnGEQ x y       -> pr x ++ " ≥ " ++ pr y
      BlnLE x y        -> pr x ++ " < " ++ pr y
      BlnLEQ x y       -> pr x ++ " < " ++ pr y
      BlnElem x y      -> pr x ++ " ≤ " ++ pr y
      BlnOr x y        -> pr x ++ " ∨ " ++ pr y
      BlnAnd x y       -> pr x ++ " ∧ " ++ pr y
      BlnImpl x y      -> pr x ++ " → " ++ pr y
      BlnEquiv x y     -> pr x ++ " ↔ " ++ pr y
      LtlRNext x y     -> "X [ " ++ pr x ++ " ] " ++ pr y
      LtlRGlobally x y -> "G [ " ++ pr x ++ " ] " ++ pr y
      LtlRFinally x y  -> "F [ " ++ pr x ++ " ] " ++ pr y
      LtlUntil x y     -> pr x ++ " U " ++ pr y
      LtlWeak x y      -> pr x ++ " W " ++ pr y
      LtlRelease x y   -> pr x ++ " R " ++ pr y
      Colon x y        -> pr x ++ " : " ++ pr y
      Pattern x y      -> pr x ++ " ~ " ++ pr y
      SetRange x y z   -> "[ " ++ pr x ++ ", " ++ pr y ++ " .. " ++ pr z ++ " ]"
      SetExplicit []     -> "∅"
      SetExplicit [x]    -> "{ " ++ pr x ++ " }"
      SetExplicit (x:xr) -> "{ " ++ pr x ++ concatMap ((',':) . (' ':) . pr) xr ++ "}"
      NumRPlus xs x    -> "Σ [ " ++ concatMap (flip (++) " " . pr) xs ++ "] " ++ pr x
      NumRMul xs x     -> "Π [" ++ concatMap (flip (++) " " . pr) xs ++ "] " ++ pr x
      SetRCup xs x     -> "⋃ [" ++ concatMap (flip (++) " " . pr) xs ++ "] " ++ pr x
      SetRCap xs x     -> "⋂ [" ++ concatMap (flip (++) " " . pr) xs ++ "] " ++ pr x
      BlnROr xs x      -> "⋁ [" ++ concatMap (flip (++) " " . pr) xs ++ "] " ++ pr x
      BlnRAnd xs x     -> "⋀ [ " ++ concatMap (flip (++) " " . pr) xs ++ "] " ++ pr x

-----------------------------------------------------------------------------
