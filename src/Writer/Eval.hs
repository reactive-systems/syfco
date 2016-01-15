{-# LANGUAGE FlexibleContexts #-}
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

import Data.List
    ( find
    )  

import Data.Char
    ( toLower
    )

import Data.Types
    ( Semantics(..)
    )      

import Data.LTL
    ( Atomic(..)
    , Formula(..)
    , applyAtomic
    , subFormulas  
    , applySub       
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
    , argsError  
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
    , foldM
    , execStateT
    , evalStateT
    , liftM2        
    , liftM
    , when 
    , get
    , put
    )

import Control.Exception
    ( assert
    )  

    
import Data.Array.IArray
    ( (!)
    )  

import qualified Data.Graph as G

import qualified Data.IntMap as IM

import qualified Data.Set as S

import qualified Data.Array.IArray as A

import Debug.Trace

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
  :: Configuration -> Specification
     -> Either Error ([Formula],[Formula],[Formula])

eval c s = do
  s' <- foldM overwriteParameter s $ owParameter c
  let
    s'' = s' {
      target = case (owTarget c) of
         Nothing -> target s'
         Just t  -> t
      }
    xs = filter (isunary s'') $
         map bIdent $ parameters s'' ++ definitions s''
    ys = concatMap (\i -> map (\j -> (i,j)) $ filter (isunary s'') $
                         idDeps $ symboltable s'' ! i) xs
    minkey = foldl min (head xs) xs
    maxkey = foldl max (head xs) xs
    zs = if null xs then []
         else reverse $ G.topSort $ G.buildG (minkey,maxkey) ys
    ss = map bIdent $ inputs s'' ++ outputs s''

  stt <- execStateT (mapM_ staticBinding zs) $
        ST (symboltable s'') IM.empty $ busDelimiter c
  sti <- execStateT (mapM_ componentSignal ss) stt

  as <- evalStateT (mapM evalLtl $ assumptions s'') sti
  is <- evalStateT (mapM evalLtl $ invariants s'') sti
  gs <- evalStateT (mapM evalLtl $ guarantees s'') sti

  return $ splitConjuncts $ overwrite s'' 
    (map plainltl as, map plainltl is, map plainltl gs)

  where
    isunary y x = null $ idArgs $ symboltable y ! x

    plainltl = applyAtomic revert . vltl

    revert :: Atomic -> Formula
    revert x = Atomic $ case x of
      Input y  -> Input $ lower $ last $ words y
      Output y -> Output $ lower $ last $ words y

    lower x =
      if needsLower (outputFormat c) 
      then map toLower x
      else x

    overwrite sp (as,is,gs) =
      let
        og = all outputsGuarded (as ++ is ++ gs)
        ig = all inputsGuarded (as ++ is ++ gs)        
      in (map (adjustOW og ig sp) as,
          map (adjustOW og ig sp) is,
          map (adjustOW og ig sp) gs)

    adjustOW og ig sp e = case owSemantics c of
      Nothing -> e
      Just m -> case (semantics sp, m) of
        (SemanticsMealy, SemanticsMealy) -> e
        (SemanticsMoore, SemanticsMoore) -> e                                        
        (SemanticsStrictMealy, SemanticsStrictMealy) -> e
        (SemanticsStrictMoore, SemanticsStrictMoore) -> e          
        (SemanticsMealy, SemanticsMoore) ->
          if ig then unGuardInputs sp e else guardOutputs sp e
        (SemanticsStrictMealy, SemanticsStrictMoore) ->
          if ig then unGuardInputs sp e else guardOutputs sp e          
        (SemanticsMoore, SemanticsMealy) -> 
          if og then unGuardOutputs sp e else guardInputs sp e
        (SemanticsStrictMoore, SemanticsStrictMealy) ->
          if og then unGuardOutputs sp e else guardInputs sp e
        _ -> error "TODO no converions between strict, non-strict yet"

    outputsGuarded e = case e of
      Next (Atomic (Output _)) -> True
      Atomic (Output _) -> False
      _ -> all outputsGuarded $ subFormulas e

    inputsGuarded e = case e of
      Next (Atomic (Input _)) -> True
      Atomic (Input _) -> False
      _ -> all inputsGuarded $ subFormulas e      

    guardOutputs sp e = case e of
      Atomic (Output x) -> Next $ Atomic $ Output x
      _        -> applySub (guardOutputs sp) e

    guardInputs sp e = case  e of
      Atomic (Input x) -> Next $ Atomic $ Input x
      _        -> applySub (guardInputs sp) e      

    unGuardOutputs sp e  = case e of
      Next (Atomic (Output x)) -> Atomic $ Output x
      _                        -> applySub (unGuardOutputs sp) e

    unGuardInputs sp e  = case e of
      Next (Atomic (Input x)) -> Atomic $ Input x
      _                       -> applySub (unGuardInputs sp) e

    splitConjuncts (xs,ys,zs) =
      (concatMap splitC xs, concatMap splitC ys, concatMap splitC zs)

    splitC fml = case fml of
      And xs -> concatMap splitC xs
      _      -> [fml]  

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
      _                -> assert False undefined
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
  _               -> assert False undefined

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
  _                -> assert False undefined
    
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
    Nothing -> assert False undefined
    
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
      _           -> assert False undefined
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
  _             -> assert False undefined

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
  _             -> assert False undefined

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
    _     -> assert False undefined
  (Or xs, BlnOr z v)            -> case xs of
    [x,y] -> binary x y z v
    _     -> assert False undefined
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
  _ -> assert False undefined

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
          _ -> assert False undefined
        BlnLE e _   -> case expr e of
          BlnLE _ m -> case expr m of
            BaseId r -> r
            _ -> assert False undefined
          BlnLEQ _ m -> case expr m of
            BaseId r -> r
            _ -> assert False undefined
          _ -> assert False undefined
        BlnLEQ e _  -> case expr e of
          BlnLE _ m -> case expr m of
            BaseId r -> r
            _ -> assert False undefined
          BlnLEQ _ m -> case expr m of
            BaseId r -> r
            _ -> assert False undefined
          _ -> assert False undefined
        _ -> assert False undefined
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
  _ -> assert False undefined

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
  _ -> assert False undefined

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
  _      -> assert False undefined

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
  VEmpty -> assert False undefined

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

-- | Overwrites a given parameter by the given new value.

overwriteParameter
  :: Specification -> (String, Int) -> Either Error Specification

overwriteParameter s (n,v) =
  case find ((n ==) . idName . (symboltable s !) . bIdent) $ parameters s of
  Nothing -> argsError $ "Specification has no parameter: " ++ n 
  Just b  -> do
    let b' = b {
          bVal = if null $ bVal b
                 then []
                 else [ Expr (BaseCon v) $ srcPos $ head $ bVal b ]
          }
    return s {
      parameters = map (replace b') $ parameters s,
      symboltable = updSymTable b' $ symboltable s
      }

  where
    replace b' b =
      if bIdent b == bIdent b'
      then b' else b

    updSymTable y t =
      A.amap (\x -> if idName x /= idName (t ! (bIdent y)) then x else x {
                 idBindings =
                    if null $ bVal y 
                    then Expr (SetExplicit []) $ bPos y
                    else Expr (SetExplicit [head $ bVal y]) $ bPos y
                 }) t
      

-----------------------------------------------------------------------------                   

