-----------------------------------------------------------------------------
-- |
-- Module      :  Data.LookupTable
-- Description :  Data type to store all identifier specific information
-- License     :  MIT (see the LICENSE file)
-- 
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
-- 
-- Data type to store all identifier specific information
-- 
-----------------------------------------------------------------------------

module Data.LookupTable
    ( LookupTable
    , IdRec(..)
    , ltToCSV
    ) where

-----------------------------------------------------------------------------

import Data.Types
   ( IdType(..)
   )
   
import Data.Expression
   ( Expr
   , ExprPos(..)
   , SrcPos(..)
   , prExpr
   )

import Data.Char
   ( ord
   , chr
   )  

import Data.Array
   ( Array
   , assocs  
   )

-----------------------------------------------------------------------------

-- | A lookup table is an array mapping identifieres, represend by integers,
-- to blocks of information.

type LookupTable = Array Int IdRec

-----------------------------------------------------------------------------

-- | Each block contains:
-- 
--     * The name of the identifier
-- 
--     * The position of the identifer definition in the source file
-- 
--     * The arguemnts, in case the identifier describes a function
-- 
--     * The expression, the identifier is bound to
-- 
--     * The type of the identifier
-- 
--     * The list of identifiers, which have to be evaluated first to
--       evaluate this identifier

data IdRec =
  IdRec
  { idName :: String
  , idPos :: ExprPos
  , idArgs :: [Int]
  , idBindings :: Expr Int
  , idType :: IdType
  , idDeps :: [Int]
  }

-----------------------------------------------------------------------------

-- | Prints a lookup table in the CVS format (for debugging purposes only).

ltToCSV
  :: LookupTable -> IO ()

ltToCSV lt = do
    putStrLn "Id;Name;Position;Arguments;Binding;Type;Dependencies;"
    mapM_ printEntry $ assocs lt

  where
    printEntry (i,r) = do
      putStr $ show i
      putStr ";"
      putStr $ idName r
      putStr ";"
      putStr $ prExprPos $ idPos r
      putStr ";"
      putStr $ commasepxs $ idArgs r
      putStr ";"
      putStr $ prExpr $ idBindings r
      putStr ";"
      putStr $ prType $ idType r
      putStr ";"
      putStr $ commasepxs $ idDeps r
      putStrLn ";"

    commasepxs xs = case xs of
      (x:xr) -> show x ++ (concatMap ((:) ',') $ map show xr)
      []     -> ""

    prExprPos pos =
      let 
        bl = srcLine $ srcBegin pos
        bc = srcColumn $ srcBegin pos
        el = srcLine $ srcEnd pos
        ec = srcColumn $ srcEnd pos
      in
        "(" ++ show bl ++ "," ++ show (bc - 1) ++
        if bl == el then
          "-" ++ show (ec - 2) ++ ")"
        else
          show el ++ ":" ++ show ec ++ ")"
    
    prType t = case t of
      TNumber   -> "number"
      TSignal x -> show (TSignal x)
      TLtl      -> "ltl"
      TBoolean  -> "bool"
      TPattern  -> "pattern"
      TEmptySet -> "empty set"
      TSet x    -> prType x ++ " set"
      TPoly i   ->
        if i >= ord 'a' && i <= ord 'z'
        then [chr i]
        else "a" ++ show i

-----------------------------------------------------------------------------                

