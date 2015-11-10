-----------------------------------------------------------------------------
-- |
-- Module      :  Writer.Eval
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
-- 
-- Unfolds all high level constructs of a specification to the corresponding
-- low level constructs.
-- 
-----------------------------------------------------------------------------

module Writer.Eval
    ( eval
    ) where

-----------------------------------------------------------------------------

import Utils
    ( iter
    )
    
import Config
    ( Configuration(..)
    )

import Data.Char
    ( toLower
    )  

import Data.LTL
    ( Atomic(..)
    , Formula(..)
    , applyAtomic  
    )
    
import Data.Types
    ( IdType(..)
    , SignalType(..)
    )
    
import Data.Binding
    ( BindExpr(..)
    )
    
import Data.Expression
    ( Expr(..)
    , Expr'(..)
    , ExprPos  
    )
    
import Data.Specification
    ( Specification(..)
    )
    
import Data.SymbolTable
    ( IdRec(..)
    , SymbolTable
    )  

import Writer.Error
    ( Error
    , errBounds
    , errMinSet
    , errMaxSet
    , errSetCap  
    , errNoMatch  
    )

import Writer.Formats
    ( needsLower
    )  

import Control.Monad.State
    ( StateT(..)
    , execStateT
    , evalStateT
    , liftM2        
    , liftM
    , when 
    , get
    , put
    )
    
import Data.Array.IArray
    ( (!)
    )  

import qualified Data.Graph as G

import qualified Data.IntMap as IM

import qualified Data.Set as S

-----------------------------------------------------------------------------

data Value =
    VNumber Int
  | VLtl Formula
  | VSet (S.Set Value)
  | VEmpty
  deriving (Eq,Show)

-----------------------------------------------------------------------------

instance Ord Value where
  compare x y = case (x,y) of
    (VEmpty, _)            -> LT
    (_, VEmpty)            -> GT
    (VNumber a, VNumber b) -> compare a b
    (VNumber _, _)         -> LT
    (VLtl a, VLtl b)       -> compare a b
    (VLtl _, VNumber _)    -> GT
    (VLtl _, VSet _)       -> LT
    (VSet a, VSet b)       -> compare a b
    (VSet _, _)            -> GT

-----------------------------------------------------------------------------

type Evaluator a = a -> StateT ST (Either Error) Value

-----------------------------------------------------------------------------

data ST = ST
  { tLookup :: SymbolTable
  , tValues :: IM.IntMap Value
  , delimiter :: String  
  }

-----------------------------------------------------------------------------

-- | @eval d s@ Evaluates all high level constructs of the given
-- specification @s@. Thereby, signals which are obtaind by bus
-- accesses are printed using the delimiter string given by @d@.

eval
  :: Configuration -> Specification -> Either Error ([Formula],[Formula],[Formula])

eval c s = do
  let
    xs = filter isunary $ map bIdent $ parameters s ++ definitions s
    ys = concatMap (\i -> map (\j -> (i,j)) $ filter isunary $
                         idDeps $ symboltable s ! i) xs
    minkey = foldl min (head xs) xs
    maxkey = foldl max (head xs) xs
    zs = if null xs then []
         else reverse $ G.topSort $ G.buildG (minkey,maxkey) ys
    ss = map bIdent $ inputs s ++ outputs s

  stt <- execStateT (mapM_ staticBinding zs) $
        ST (symboltable s) IM.empty $ busDelimiter c
  sti <- execStateT (mapM_ componentSignal ss) stt
  as <- evalStateT (mapM evalLtl $ assumptions s) sti
  is <- evalStateT (mapM evalLtl $ invariants s) sti
  gs <- evalStateT (mapM evalLtl $ guarantees s) sti
  
  return (map plainltl as, map plainltl is, map plainltl gs)

  where
    isunary x = null $ idArgs $ symboltable s ! x

    plainltl = applyAtomic revert . vltl

    revert :: Atomic -> Formula
    revert x = Atomic $ case x of
      Input y  -> Input $ lower $ last $ words y
      Output y -> Output $ lower $ last $ words y

    lower x =
      if needsLower (outputFormat c) 
      then map toLower x
      else x
                
-----------------------------------------------------------------------------

staticBinding
  :: Int -> StateT ST (Either Error) ()

staticBinding x = do
  st <- get
  
  VSet bs <- evalSet $ idBindings $ tLookup st ! x

  let v = head $ S.toList bs
  put $ st {
    tValues = IM.insert x v $ tValues st
    }

-----------------------------------------------------------------------------

componentSignal
  :: Int -> StateT ST (Either Error) ()

componentSignal i = do
  st <- get
  let
    c = case idType $ tLookup st ! i of
      TSignal STInput  -> Input
      TSignal STOutput -> Output
      _                -> error "internal error (ERR 02)"
    n = show i ++ " " ++ idName (tLookup st ! i)
  put $ st {
    tValues = IM.insert i (VLtl $ Atomic $ c n) $ tValues st
    }

-----------------------------------------------------------------------------

evalExpr
  :: Evaluator (Expr Int)

evalExpr e = case expr e of
  BaseWild        -> evalLtl e
  BaseCon {}      -> evalNum e
  NumSMin {}      -> evalNum e
  NumSMax {}      -> evalNum e
  NumSSize {}     -> evalNum e
  NumSizeOf {}    -> evalNum e
  NumPlus {}      -> evalNum e
  NumMinus {}     -> evalNum e
  NumMul {}       -> evalNum e
  NumDiv {}       -> evalNum e
  NumMod {}       -> evalNum e
  NumRPlus {}     -> evalNum e
  NumRMul {}      -> evalNum e
  BaseTrue        -> evalLtl e
  BaseFalse       -> evalLtl e
  BaseBus {}      -> evalLtl e
  BlnEQ {}        -> evalLtl e
  BlnNEQ {}       -> evalLtl e
  BlnGE {}        -> evalLtl e
  BlnGEQ {}       -> evalLtl e
  BlnLE {}        -> evalLtl e
  BlnLEQ {}       -> evalLtl e
  BlnNot {}       -> evalLtl e
  BlnOr {}        -> evalLtl e
  BlnROr {}       -> evalLtl e
  BlnAnd {}       -> evalLtl e
  BlnRAnd {}      -> evalLtl e
  BlnImpl {}      -> evalLtl e
  BlnElem {}      -> evalLtl e
  BlnEquiv {}     -> evalLtl e
  LtlNext {}      -> evalLtl e
  LtlRNext {}     -> evalLtl e
  LtlGlobally {}  -> evalLtl e
  LtlRGlobally {} -> evalLtl e
  LtlFinally {}   -> evalLtl e
  LtlRFinally {}  -> evalLtl e
  LtlUntil {}     -> evalLtl e
  LtlWeak {}      -> evalLtl e
  LtlRelease {}   -> evalLtl e
  SetExplicit {}  -> evalSet e
  SetRange {}     -> evalSet e
  SetCup {}       -> evalSet e
  SetRCup {}      -> evalSet e
  SetCap {}       -> evalSet e
  SetRCap {}      -> evalSet e
  SetMinus {}     -> evalSet e
  BaseId x        -> idValue x
  BaseFml xs x    -> fmlValue xs (srcPos e) x
  Colon {}        -> evalColon e  
  _               -> error "internal error (ERR_03)"

-----------------------------------------------------------------------------

evalLtl
  :: Evaluator (Expr Int)

evalLtl e = case expr e of
  BaseTrue         -> return $ VLtl TTrue
  BaseFalse        -> return $ VLtl FFalse  
  BlnNot x         -> liftMLtl Not x  
  LtlNext x        -> liftMLtl Next x
  LtlGlobally x    -> liftMLtl Globally x
  LtlFinally x     -> liftMLtl Finally x
  LtlUntil x y     -> liftM2Ltl Until x y
  LtlRelease x y   -> liftM2Ltl Release x y
  LtlWeak x y      -> liftM2Ltl Weak x y
  BlnImpl x y      -> liftM2Ltl Implies x y
  BlnEquiv x y     -> liftM2Ltl Equiv x y  
  BlnEQ x y        -> liftM2Num (==) x y 
  BlnNEQ x y       -> liftM2Num (/=) x y 
  BlnGE x y        -> liftM2Num (>) x y
  BlnGEQ x y       -> liftM2Num (>=) x y
  BlnLE x y        -> liftM2Num (<) x y
  BlnLEQ x y       -> liftM2Num (<=) x y
  BaseId _         -> do
    VLtl x <- evalExpr e
    return $ VLtl x
  BaseFml _ _      -> do
    VLtl x <- evalExpr e
    return $ VLtl x
  LtlRNext x y     -> do
    VNumber n <- evalNum x
    VLtl v <- evalLtl y
    return $ VLtl $ iter Next n v
  LtlRGlobally x y -> do
    (i,j) <- evalRange x
    if i > j then
      return $ VLtl TTrue
    else do
      VLtl v <- evalLtl y      
      return $ VLtl $ iter Next i $ 
        iter (\a -> And [v, Next a]) (j - i) v
  LtlRFinally x y  -> do
    (i,j) <- evalRange x
    if i > j then
      return $ VLtl TTrue
    else do
      VLtl v <- evalLtl y      
      return $ VLtl $ iter Next i $ 
        iter (\a -> Or [v, Next a]) (j - i) v

  BlnElem x y      -> do
    a <- evalExpr x
    VSet b <- evalExpr y
    return $ VLtl $ if S.member a b then 
      TTrue 
    else
      FFalse
  BlnOr x y        -> do
    VLtl a <- evalLtl x
    VLtl b <- evalLtl y
    return $ VLtl $ Or [a,b]
  BlnAnd x y       -> do
    VLtl a <- evalLtl x
    VLtl b <- evalLtl y
    return $ VLtl $ And [a,b]
  BlnRAnd xs x     -> 
    let f = VLtl . And . map (\(VLtl v) -> v)
    in evalConditional evalLtl f xs x
  BlnROr xs x      -> 
    let f = VLtl . Or . map (\(VLtl v) -> v)
    in evalConditional evalLtl f xs x
  BaseBus x y      -> do
    VLtl (Atomic a) <- idValue y 
    VNumber b <- evalNum x
    st <- get
    VSet z <- evalExpr $ idBindings $ tLookup st ! y
    case S.toList z of
      [VNumber s] -> 
        when (b < 0 || b >= s) $ 
          errBounds (show a) s b $ srcPos e
      _           -> return ()
    return $ VLtl $ Atomic $ case a of
      Input r  -> Input (show y ++ " " ++ r ++ delimiter st ++ show b)
      Output r -> Output (show y ++ " " ++ r ++ delimiter st ++ show b)
  _                -> error "internal error (ERR_04)"
    
  where
    liftMLtl f m = do
      VLtl x <- evalLtl m
      return $ VLtl $ f x 

    liftM2Ltl f m n = do
      VLtl x <- evalLtl m
      VLtl y <- evalLtl n
      return $ VLtl $ f x y

    liftM2Num f m n = do
      VNumber x <- evalNum m
      VNumber y <- evalNum n
      return $ VLtl $ if f x y then 
        TTrue 
      else 
        FFalse

-----------------------------------------------------------------------------

idValue
  :: Evaluator Int

idValue i = do
  st <- get
  case IM.lookup i $ tValues st of
    Just x  -> return x
    Nothing -> error "internal error (ERR_05)"
    
---

evalNum
  :: Evaluator (Expr Int)

evalNum e = case expr e of
  BaseCon x     -> return $ VNumber x
  NumPlus x y   -> liftM2Num (+) x y
  NumMinus x y  -> liftM2Num (-) x y
  NumMul x y    -> liftM2Num (*) x y
  NumDiv x y    -> liftM2Num div x y
  NumMod x y    -> liftM2Num mod x y
  NumSMin x     -> do
    VSet y <- evalExpr x
    let xs = map (\(VNumber v) -> v) $ S.elems y
    if null xs then
      errMinSet $ srcPos e
    else
      return $ VNumber $ foldl min (head xs) xs
  NumSMax x     -> do
    VSet y <- evalExpr x
    let xs = map (\(VNumber v) -> v) $ S.elems y
    if null xs then
      errMaxSet $ srcPos e
    else
      return $ VNumber $ foldl max (head xs) xs
  NumSSize x    -> do
    VSet y <- evalExpr x
    let xs = map (\(VNumber v) -> v) $ S.elems y
    return $ VNumber $ length xs
  NumSizeOf x   -> do
    VLtl (Atomic y) <- evalExpr x
    let i = read $ head $ words $ case y of
              Input z  -> z
              Output z -> z
    st <- get
    VSet s <- evalExpr $ idBindings $ tLookup st ! i 
    case  S.toList s of
      [VNumber z] -> return $ VNumber z
      _           -> error "internal error (ERR_14)"
  NumRPlus xs x -> 
    let f = VNumber . sum . map (\(VNumber v) -> v)
    in evalConditional evalNum f xs x
  NumRMul xs x  -> 
    let f = VNumber . product . map (\(VNumber v) -> v)
    in evalConditional evalNum f xs x
  BaseId _      -> do
    VNumber x <- evalExpr e
    return $ VNumber x
  BaseFml _ _   -> do
    VNumber x <- evalExpr e
    return $ VNumber x
  _             -> error "internal error (ERR_06)"

  where
    liftM2Num f m n = do
      VNumber x <- evalNum m
      VNumber y <- evalNum n
      return $ VNumber $ f x y

-----------------------------------------------------------------------------

evalBool 
  :: Expr Int -> StateT ST (Either Error) Bool

evalBool e = case expr e of
  BaseOtherwise -> return True
  BaseTrue      -> return True
  BaseFalse     -> return False
  BlnNot x      -> liftM not $ evalBool x
  BlnImpl x y   -> do
    a <- evalBool x
    if a then 
      evalBool y
    else 
      return True
  BlnEquiv x y  -> liftM2 (==) (evalBool x) (evalBool y)
  BlnOr x y     -> liftM2 (||) (evalBool x) (evalBool y)
  BlnAnd x y    -> liftM2 (&&) (evalBool x) (evalBool y)
  BlnEQ x y     -> liftM2Num (==) x y
  BlnNEQ x y    -> liftM2Num (/=) x y 
  BlnGE x y     -> liftM2Num (>) x y
  BlnGEQ x y    -> liftM2Num (>=) x y
  BlnLE x y     -> liftM2Num (<) x y
  BlnLEQ x y    -> liftM2Num (<=) x y
  BlnElem x y   -> do
    a <- evalExpr x
    VSet b <- evalExpr y
    return $ S.member a b
  BlnRAnd xs x  -> evalConditional evalBool and xs x
  BlnROr xs x   -> evalConditional evalBool or xs x
  Pattern x y   -> do
    VLtl a <- evalLtl x
    checkPattern a y
  _             -> error "internal error (ERR_07)"

  where
    liftM2Num f m n = do
      VNumber x <- evalNum m
      VNumber y <- evalNum n
      return $ f x y

-----------------------------------------------------------------------------

checkPattern 
  :: Formula -> Expr Int -> StateT ST (Either Error) Bool

checkPattern f e = case (f,expr e) of
  (_, BaseWild)                     -> return True  
  (TTrue, BaseTrue)             -> return True
  (FFalse, BaseFalse)           -> return True
  (Not x, BlnNot y)             -> checkPattern x y
  (Next x, LtlNext y)           -> checkPattern x y
  (Globally x, LtlGlobally y)   -> checkPattern x y
  (Finally x, LtlFinally y)     -> checkPattern x y
  (Implies x y, BlnImpl z v)    -> binary x y z v
  (Equiv x y, BlnEquiv z v)     -> binary x y z v
  (Until x y, LtlUntil z v)     -> binary x y z v
  (Release x y, LtlRelease z v) -> binary x y z v
  (And xs, BlnAnd z v)          -> case xs of
    [x,y] -> binary x y z v
    _     -> error "internal error (ERR_08)"
  (Or xs, BlnOr z v)            -> case xs of
    [x,y] -> binary x y z v
    _     -> error "internal error (ERR_09)"
  (_, BaseId i)                     -> do
    st <- get
    put st {
      tValues = IM.insert i (VLtl f) $ tValues st
      }
    return True
  _                                 -> return False

  where
    binary x y z a = do
      b <- checkPattern x z
      if b then
        checkPattern  y a
      else
        return False

-----------------------------------------------------------------------------

evalSet
  :: Evaluator (Expr Int)

evalSet e = case expr e of
  SetExplicit xs -> do
    ys <- mapM evalExpr xs
    return $ VSet $ S.fromList ys
  SetRange x y z -> do
    [VNumber a,VNumber b,VNumber c] <- mapM evalNum [x,y,z]
    return $ VSet $ S.fromList $ map VNumber [a,b..c]
  SetCup x y     -> liftM2Set S.union x y
  SetCap x y     -> liftM2Set S.intersection x y
  SetMinus x y   -> liftM2Set S.difference x y
  SetRCup xs x   ->
    let f = VSet . S.unions . map (\(VSet v) -> v)
    in evalConditional evalSet f xs x
  SetRCap xs x   ->
    if null xs then 
      errSetCap $ srcPos e
    else
      let
        g vs = foldl S.intersection (head vs) vs
        f = VSet . g . map (\(VSet v) -> v)
      in 
        evalConditional evalSet f xs x    
  BaseId _       -> do
    VSet x <- evalExpr e
    return $ VSet x
  BaseFml _ _    -> do
    VSet x <- evalExpr e
    return $ VSet x
  _ -> error "internal error (ERR_10)"

  where
    liftM2Set f x y = do
      VSet a <- evalSet x
      VSet b <- evalSet y
      return $ VSet $ f a b

-----------------------------------------------------------------------------

evalConditional
  :: (Expr Int -> StateT ST (Either Error) a) -> ([a] -> a)
      -> [Expr Int] -> Expr Int -> StateT ST (Either Error) a

evalConditional fun f xs x =
  if null xs then
    fun x
  else do
    st <- get
    let
      i = case expr $ head xs of
        BlnElem e _ -> case expr e of
          BaseId r -> r
          _ -> error "internal error (ERR_15)"
        BlnLE e _   -> case expr e of
          BlnLE _ m -> case expr m of
            BaseId r -> r
            _ -> error "internal error (ERR_16)"
          BlnLEQ _ m -> case expr m of
            BaseId r -> r
            _ -> error "internal error (ERR_17)"
          _ -> error "internal error (ERR_18)"
        BlnLEQ e _  -> case expr e of
          BlnLE _ m -> case expr m of
            BaseId r -> r
            _ -> error "internal error (ERR_19)"
          BlnLEQ _ m -> case expr m of
            BaseId r -> r
            _ -> error "internal error (ERR_20)"
          _ -> error "internal error (ERR_21)"
        _ -> error "internal error (ERR_22)"
      s = idBindings $ tLookup st ! i
    VSet vs <- evalSet s
    rs <- mapM (bindExec i (tail xs) x) $ S.toList vs
    return $ f rs

  where
    bindExec i xr e v = do
      st <- get
      put st {
        tValues = IM.insert i v $ tValues st
        }
      r <- evalConditional fun f xr e
      put st
      return r

-----------------------------------------------------------------------------

evalRange
  :: Expr Int -> StateT ST (Either Error) (Int,Int)
           

evalRange e = case expr e of
  Colon x y -> do
    VNumber a <- evalNum x
    VNumber b <- evalNum y
    return (a,b)
  _ -> error "internal error (ERR_11)"

-----------------------------------------------------------------------------

evalColon
  :: Evaluator (Expr Int)

evalColon e = case expr e of
  Colon x y -> do
    st <- get
    a <- evalBool x
    if a then do
      r <- evalExpr y
      put st
      return r
    else do
      put st
      return VEmpty
  _ -> error "internal error (ERR_12)"

-----------------------------------------------------------------------------

fmlValue
  :: [Expr Int] -> ExprPos -> Evaluator Int

fmlValue args p i = do
  st <- get  
  as <- mapM evalExpr args
  let xs = zip as $ idArgs $ tLookup st ! i    
  put st {
    tValues = foldl (\m (v,j) -> IM.insert j v m) (tValues st) xs
    }
  VSet a <- evalSet $ idBindings $ tLookup st ! i
  put st
  let ys = filter (/= VEmpty) $ S.toList a
  if null ys then 
    errNoMatch (idName $ tLookup st ! i) (map (prVal . fst) xs) p
  else
    return $ head ys

-----------------------------------------------------------------------------

vltl
  :: Value -> Formula

vltl x = case x of
  VLtl y -> y
  _      -> error "internal error (ERR_13)"

-----------------------------------------------------------------------------

prVal
  :: Value -> String

prVal v = case v of
  VNumber x -> show x
  VLtl x -> asciiLTL x
  VSet x -> case S.toList x of
    []     -> "{}"
    (y:yr) -> "{ " ++ prVal y ++
             concatMap ((:) ',' . (:) ' ' . prVal) yr ++ " }"
  VEmpty -> error "internal error"

-----------------------------------------------------------------------------

asciiLTL
  :: Formula -> String

asciiLTL fml = case fml of
  TTrue                   -> ptrue
  FFalse                  -> pfalse
  Not (Atomic (Input x))  -> pnot ++ prUO' (Atomic (Input x))
  Not (Atomic (Output x)) -> pnot ++ prUO' (Atomic (Output x))      
  Atomic (Input x)        -> x
  Atomic (Output x)       -> x      
  Not x                   -> pnot ++ prUO' x 
  And []                  -> asciiLTL TTrue
  And [x]                 -> asciiLTL x
  And (x:xr)              -> prAnd' x ++ concatMap (((" " ++ pand ++ " ") ++)
                                                    . prAnd') xr 
  Or []                   -> asciiLTL FFalse
  Or [x]                  -> asciiLTL x  
  Or (x:xr)               -> prOr' x ++ concatMap (((" " ++ por ++ " ") ++)
                                                   . prOr') xr
  Implies x y             -> prOr' x ++ " " ++ pimplies ++ " " ++ prOr' y
  Equiv x y               -> prOr' x ++ " " ++ pequiv ++ " " ++ prOr' y
  Next x                  -> pnext ++ prUO' x    
  Globally x              -> pglobally ++ prUO' x
  Finally x               -> pfinally ++ prUO' x
  Until x y               -> prOr' x ++ " " ++ puntil ++ " " ++ prOr' y
  Release x y             -> prOr' x ++ " " ++ prelease ++ " " ++ prOr' y
  Weak x y                -> prOr' x ++ " " ++ pweak ++ " " ++ prOr' y

  where
    parens x = "(" ++ x ++ ")"

    prUO p f = case f of
      And _       -> parens $ p f
      Or _        -> parens $ p f
      Implies _ _ -> parens $ p f
      Equiv _ _   -> parens $ p f
      Until _ _   -> parens $ p f
      Release _ _ -> parens $ p f
      _              -> p f

    prAnd p f = case f of
      Or _        -> parens $ p f
      Implies _ _ -> parens $ p f
      Equiv _ _   -> parens $ p f
      Until _ _   -> parens $ p f
      Release _ _ -> parens $ p f
      _              -> p f      

    prOr p f = case f of
      Implies _ _ -> parens $ p f
      Equiv _ _   -> parens $ p f
      Until _ _   -> parens $ p f
      Release _ _ -> parens $ p f
      _              -> p f      
    
    prUO' =  prUO asciiLTL
    prAnd' = prAnd asciiLTL
    prOr' = prOr asciiLTL      

    ptrue = "true" 
    pfalse = "false"
    pnot = "!" 
    pand = "&&" 
    por = "||" 
    pimplies = "->" 
    pequiv = "<->" 
    pnext = "X" 
    pglobally = "F"
    pfinally = "G" 
    puntil = "U" 
    prelease = "R" 
    pweak = "W"

-----------------------------------------------------------------------------
