module Data.LookupTable
       ( LookupTable
       , IdRec(..)
       , printLT
       ) where

---

import Data.Types
import Data.Expression
import Data.Array 

---

type LookupTable = Array Int IdRec

---

data IdRec =
  IdRec
  { idName :: String
  , idPos :: ExprPos
  , idArgs :: [Int]
  , idBindings :: Expr Int
  , idType :: IdType
  , idDeps :: [Int]
  }

---

printLT
  :: LookupTable -> IO ()

printLT lt =
  let
    li = foldl max 0 $ map (length . show . fst) $ assocs lt
    ln = foldl max 0 $ map (length . idName . snd) $ assocs lt
    lp = foldl max 0 $ map (length . prExprPos . idPos . snd) $ assocs lt
    la = foldl max 0 $ map (length . commasepxs . idArgs . snd) $ assocs lt
    lb = foldl max 0 $ map (length . prExpr .  idBindings . snd) $ assocs lt
    ly = foldl max 0 $ map (length . prType . idType . snd) $ assocs lt
    ld = foldl max 0 $ map (length . commasepxs . idDeps . snd) $ assocs lt
  in
    mapM_ (printRec li ln lp la lb ly ld) $ assocs lt

  where
    printRec a b c d e f g (i,r) = do
      putStr " "
      putStr $ filll a $ show i
      putStr " | "
      putStr $ fillr b $ idName r
      putStr " | "
      putStr $ fillr c $ prExprPos $ idPos r
      putStr " | "
      putStr $ fillr d $ commasepxs $ idArgs r
      putStr " | "
      putStr $ fillr e $ prExpr $ idBindings r
      putStr " | "
      putStr $ fillr f $ prType $ idType r
      putStr " | "
      putStr $ fillr g $ commasepxs $ idDeps r
      putStrLn " |"

    filll x y = replicate (x - length y) ' ' ++ y
    fillr x y = y ++ replicate (x - length y) ' '
      
---

prExprPos
  :: ExprPos -> String

prExprPos pos =
  "(" ++ show bl ++ "," ++ show (bc - 1) ++
  if bl == el then
    "-" ++ show (ec - 2) ++ ")"
  else
    show el ++ ":" ++ show ec ++ ")"

  where
    bl = srcLine $ srcBegin pos
    bc = srcColumn $ srcBegin pos
    el = srcLine $ srcEnd pos
    ec = srcColumn $ srcEnd pos

---

commasepxs
  :: Show a => [a] -> String

commasepxs xs = case xs of
  (x:xr) -> show x ++ (concatMap ((:) ',') $ map show xr)
  []     -> ""

---

prType
  :: IdType -> String

prType t = case t of
  TPoly i -> case i of
    0 -> "a"
    1 -> "b"
    2 -> "c"
    3 -> "d"
    _ -> "a" ++ show i
  TNumber -> "number"
  TSignal x -> show (TSignal x)
  TLtl -> "ltl"
  TBoolean -> "bool"
  TPattern -> "pattern"
  TEmptySet -> "empty set"
  TSet x -> prType x ++ " set"
  
---    
