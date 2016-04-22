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
    , evalSignals  
    ) where

-----------------------------------------------------------------------------

import Utils
    ( iter
    )

import Data.Enum
    ( EnumDefinition(..)
    )

import Data.Maybe
    ( fromMaybe
    , catMaybes  
    )
    
import Config
    ( Configuration(..)
    )

import Data.Either
    ( partitionEithers
    )  

import Data.List
    ( find
    )  

import Data.Char
    ( toLower
    )

import Data.LTL
    ( Atomic(..)
    , Formula(..)
    , subFormulas
    , applyAtomic  
    , applySub       
    )
    
import Data.Types
    ( IdType(..)
    , Semantics(..)
    , SignalType(..)
    , SignalDecType(..)  
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
    , errBusCmp  
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

-----------------------------------------------------------------------------

data Value =
    VEmpty  
  | VNumber Int
  | VLtl Formula
  | VSet (S.Set Value)
  | VEnum String Int [Int -> Either Bool ()]
  | VSignal SignalType String
  | VBus SignalType Int String 

instance Show Value where
  show v = case v of
    VEmpty      -> "VEmpty"
    VNumber i   -> "VNumber " ++ show i
    VLtl f      -> "VLtl " ++ show f
    VSet s      -> "VSet " ++ show s
    VEnum {}    -> "VEnum"
    VSignal _ s -> "VSingal " ++ s
    VBus _ i s  -> "VBus " ++ s ++ "[" ++ show i ++ "]"

instance Eq Value where
  (==) VEmpty VEmpty                 = True
  (==) (VNumber i) (VNumber j)       = i == j
  (==) (VLtl f) (VLtl f')            = f == f'
  (==) (VSet s) (VSet s')            = s == s'
  (==) (VSignal _ s) (VSignal _ s')  = s == s'
  (==) (VBus _ _ s) (VBus _ _ s')    = s == s'
  (==) (VEnum _ i xs) (VEnum _ j ys) =
     i == j && length xs == length ys &&
     map (\f -> map f [0,1..i-1]) xs ==
     map (\f -> map f [0,1..i-1]) ys     
  (==) _ _                           = False

instance Ord Value where
  compare VEmpty VEmpty                 = EQ
  compare (VNumber i) (VNumber j)       = compare i j
  compare (VLtl f) (VLtl f')            = compare f f'
  compare (VSet s) (VSet s')            = compare s s'
  compare (VSignal _ s) (VSignal _ s')  = compare s s'
  compare (VBus _ _ s) (VBus _ _ s')    = compare s s'    
  compare (VEnum _ i xs) (VEnum _ j ys)
    | i /= j                 = compare i j 
    | length xs /= length ys = compare (length xs) (length ys)
    | otherwise             = 
        compare (map (\f -> map f [0,1..i-1]) xs)  
                (map (\f -> map f [0,1..i-1]) ys)
  compare x y                           = compare (cidx x) (cidx y)

    where
      cidx :: Value -> Int
      cidx VEmpty     = 0
      cidx VNumber {} = 1
      cidx VLtl {}    = 2
      cidx VSet {}    = 3
      cidx VEnum {}   = 4
      cidx VSignal {} = 5
      cidx VBus {}    = 6

-----------------------------------------------------------------------------

type Evaluator a = a -> StateT ST (Either Error) Value

-----------------------------------------------------------------------------

data ST = ST
  { tLookup :: SymbolTable
  , tValues :: IM.IntMap Value
  , delimiter :: String
  , enums :: [EnumDefinition Int]  
  }

-----------------------------------------------------------------------------

-- | @eval c s@ evaluates all high level constructs of the given
-- specification @s@ under the current configuration @c@.

eval
  :: Configuration -> Specification
     -> Either Error ([Formula],[Formula],[Formula],[Formula],[Formula],[Formula])

eval c s = do
  (s', st0, xs) <- initialize c s
  let signals = inputs s' ++ outputs s'
  
  stt <- execStateT (mapM_ staticBinding xs) st0
  (rr,sti) <- runStateT (mapM componentSignal signals) stt
  let (er,sr) = partitionEithers $ catMaybes rr

  es <- evalStateT (mapM evalLtl $ initially s') sti  
  ts <- evalStateT (mapM evalLtl $ preset s') sti
  rs <- evalStateT (mapM evalLtl $ requirements s') sti  
  as <- evalStateT (mapM evalLtl $ assumptions s') sti
  is <- evalStateT (mapM evalLtl $ invariants s') sti
  gs <- evalStateT (mapM evalLtl $ guarantees s') sti

  return $ splitConjuncts $ overwrite s' 
    ( map plainltl es, map plainltl ts, map plainltl (rs ++ er),
      map plainltl (as ++ sr), map plainltl is, map plainltl gs )

  where
    plainltl = applyAtomic apA . vltl

    apA :: Atomic -> Formula
    apA x = Atomic $ case x of
      Input y  -> Input $ lower $ last $ words y
      Output y -> Output $ lower $ last $ words y

    lower x =
      if needsLower (outputFormat c) 
      then map toLower x
      else x

    overwrite sp (es,ss,rs,as,is,gs) =
      let
        og = all outputsGuarded (es ++ ss ++ rs ++ as ++ is ++ gs)
        ig = all inputsGuarded (es ++ ss ++ rs ++ as ++ is ++ gs)
      in (map (adjustOW og ig sp) es,
          map (adjustOW og ig sp) ss,
          map (adjustOW og ig sp) rs,
          map (adjustOW og ig sp) as,
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

    splitConjuncts (es,ss,rs,xs,ys,zs) =
      ( concatMap splitC es, 
        concatMap splitC ss,
        concatMap splitC rs,         
        concatMap splitC xs,
        concatMap splitC ys,
        concatMap splitC zs)

    splitC fml = case fml of
      And xs -> concatMap splitC xs
      _      -> [fml]  

-----------------------------------------------------------------------------


-- | @evalSignals c s@ evaluates all signals of the given
-- specification @s@ under the current configuration @c@.

evalSignals
  :: Configuration -> Specification
  -> Either Error ([String],[String])

evalSignals c s = do
  (s',st,xs) <- initialize c s
  let signals = inputs s' ++ outputs s'  
  stt <- execStateT (mapM_ staticBinding xs) st
  stf <- execStateT (mapM_ componentSignal signals) stt
  let
    is = map getId $ inputs s'
    os = map getId $ outputs s'

  iv <- evalStateT (mapM getV is) stf
  ov <- evalStateT (mapM getV os) stf

  return (concat iv, concat ov)
  
  where
    getId v = case v of
      SDSingle (i,_) -> i
      SDBus (i,_) _  -> i
      SDEnum (i,_) _ -> i

    getV i = do
      st <- get
      case IM.lookup i (tValues st) of
        Nothing -> assert False undefined
        Just x  -> case x of
          VSignal _ y -> return [y]
          VBus _ n y  -> return [y ++ delimiter st ++ show j | j <- [0,1..n-1]]
          _           -> assert False undefined

-----------------------------------------------------------------------------

-- | Computes the initial state for the evaluation of the
-- specification, as well as the adapted specification and the
-- resolution order.

initialize
  :: Configuration -> Specification -> Either Error (Specification, ST, [Int])

initialize c s = do
  s' <- foldM overwriteParameter s $ owParameter c
  let
    s'' = s' {
      target = fromMaybe (target s') $ owTarget c
      }
    xs = filter (isunary s'') $
         map bIdent $ parameters s'' ++ definitions s''
    ys = concatMap (\i -> map (\j -> (i,j)) $ filter (isunary s'') $
                         idDeps $ symboltable s'' ! i) xs
    minkey = foldl min (head xs) xs
    maxkey = foldl max (head xs) xs
    zs = if null xs then []
         else reverse $ G.topSort $ G.buildG (minkey,maxkey) ys

  st <- execStateT (mapM_ enumBinding $ enumerations s) $
        ST (symboltable s'')
        IM.empty
        (busDelimiter c)
        (enumerations s)

  return (s'', st, zs)      

  where
    isunary y x = null $ idArgs $ symboltable y ! x

-----------------------------------------------------------------------------    

enumBinding
  :: EnumDefinition Int -> StateT ST (Either Error) ()

enumBinding x = do
  st <- get
  let n = idName $ tLookup st ! eName x
  put $ st {
    tValues = foldl (add n $ eSize x) (tValues st) $ eValues x
    }

  where
    add n s im (v,_,fs) = 
      IM.insert v (VEnum n s fs) im

-----------------------------------------------------------------------------      

staticBinding
  :: Int -> StateT ST (Either Error) ()

staticBinding x = do
  st <- get

  VSet bs <- evalSet $ idBindings $ tLookup st ! x

  case S.toList bs of
    []  -> return ()
    v:_ -> 
      put $ st {
        tValues = IM.insert x v $ tValues st
        }

-----------------------------------------------------------------------------

componentSignal
  :: SignalDecType Int -> StateT ST (Either Error) (Maybe (Either Value Value))

componentSignal s = do
  st <- get
  (i,v,r) <- case s of
    SDSingle (i,_) -> case idType $ tLookup st ! i of
      TSignal io  -> return (i,VSignal io $ idName (tLookup st ! i), Nothing)
      _           -> assert False undefined
    SDBus (i,_) e  -> do
      VNumber n <- evalNum e
      case idType $ tLookup st ! i of
        TBus io  -> return (i,VBus io n $ idName (tLookup st ! i), Nothing)
        _        -> assert False undefined
    SDEnum (i,_) _ ->
      case idType $ tLookup st ! i of
        TTypedBus io _ t -> case find ((== t) . eName) $ enums st of
          Nothing -> assert False undefined          
          Just e  ->
            let
              m = idName (tLookup st ! i)
              v = VBus io (eSize e) m
              r = case eMissing e of
                [] -> Nothing
                xs -> case io of
                  STInput  -> Just $ Left $
                             missing (delimiter st) m Input (eSize e) xs
                  STOutput -> Just $ Right $
                             missing (delimiter st) m Output (eSize e) xs
                  _        -> assert False undefined
            in
              return (i,v,r)

        _                -> assert False undefined

  put $ st {
    tValues = IM.insert i v $ tValues st
    }

  return r

  where
    missing d m c n xs = VLtl $ And $ map (tB d m c n) xs

    tB d m c n f = Or $ map (tV d m c f) [0,1..n-1]

    tV d m c f i = case f i of
      Left True  -> Not $ Atomic $ c $ m ++ d ++ show i
      Left False -> Atomic $ c $ m ++ d ++ show i
      _          -> assert False undefined
    
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
  BlnEQ x y        -> do
    b <- evalEquality (==) "==" x y $ srcPos e
    case b of
      Left v -> return v
      Right True -> return $ VLtl TTrue
      Right False -> return $ VLtl FFalse
  BlnNEQ x y       -> do
    b <- evalEquality (/=) "!=" x y $ srcPos e
    case b of
      Left v -> return v
      Right True -> return $ VLtl TTrue
      Right False -> return $ VLtl FFalse  
  BlnGE x y        -> liftM2Num (>) x y
  BlnGEQ x y       -> liftM2Num (>=) x y
  BlnLE x y        -> liftM2Num (<) x y
  BlnLEQ x y       -> liftM2Num (<=) x y
  BaseId _         -> do
    x <- evalExpr e
    case x of
      VLtl y             -> return $ VLtl y
      VSignal STInput y  -> return $ VLtl $ Atomic $ Input y
      VSignal STOutput y -> return $ VLtl $ Atomic $ Output y
      _                  -> assert False undefined
  BaseFml _ _      -> do
    x <- evalExpr e
    case x of
      VLtl y             -> return $ VLtl y
      VSignal STInput y  -> return $ VLtl $ Atomic $ Input y
      VSignal STOutput y -> return $ VLtl $ Atomic $ Output y
      _                  -> assert False undefined
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
    VBus io l s <- idValue y 
    VNumber b <- evalNum x

    when (b < 0 || b >= l) $
      errBounds s l b $ srcPos e

    st <- get
    return $ VLtl $ Atomic $ case io of
      STInput   -> Input $ s ++ delimiter st ++ show b
      STOutput  -> Output $ s ++  delimiter st ++ show b
      STGeneric -> assert False undefined
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
    return $ VNumber $ S.size y
  NumSizeOf x   -> do 
    VBus _ i _ <- evalExpr x
    return $ VNumber i
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
  BlnEQ x y     -> do
    b <- evalEquality (==) "==" x y $ srcPos e
    case b of
      Left _  -> assert False undefined
      Right v -> return v
  BlnNEQ x y    -> do
    b <- evalEquality (/=) "!=" x y $ srcPos e
    case b of
      Left _  -> assert False undefined
      Right v -> return v
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

evalEquality
  :: (Value -> Value -> Bool) -> String -> Expr Int -> Expr Int -> ExprPos
  -> StateT ST (Either Error) (Either Value Bool)

evalEquality eq eqs e1 e2 pos = do
  a <- evalExpr e1
  b <- evalExpr e2

  st <- get
  case (a,b) of
    (VEnum n i xs, VBus io l s)     
      | l /= i     -> errBusCmp n s eqs l i pos
      | otherwise -> return $ Left $ VLtl $ Or $
                    map (toB (delimiter st) io s [] i) xs
    (VBus io l s, VEnum n i xs)     
      | l /= i     -> errBusCmp n s eqs l i pos
      | otherwise -> return $ Left $ VLtl $ Or $
                    map (toB (delimiter st) io s [] i) xs      
    (VBus io l s, VBus io' l' s') 
      | l /= l'    -> errBusCmp s s' eqs l l' pos
      | otherwise ->
          return $ Left $ VLtl $ And 
            [ Equiv (Atomic $ cio io $ s ++ delimiter st ++ show i)
              (Atomic $ cio io' $ s' ++ delimiter st ++ show i) 
            | i <- [0,1..l-1] ]
    (VBus io l s, VSignal io' s') 
      | l /= 1     -> errBusCmp s s' eqs l 1 pos
      | otherwise -> 
          return $ Left $ VLtl $ Equiv
            (Atomic $ cio io $ s ++ delimiter st ++ "0")
            (Atomic $ cio io' s')
    (VSignal io' s', VBus io l s) 
      | l /= 1     -> errBusCmp s s' eqs l 1 pos      
      | otherwise ->
          return $ Left $ VLtl $ Equiv
            (Atomic $ cio io $ s ++ delimiter st ++ "0")
            (Atomic $ cio io' s')      
    (VEnum _ 1 [f], VSignal io' s')  ->
      return $ Left $ VLtl $ case f 0 of
        Right () -> TTrue
        Left False -> Not $ Atomic $ cio io' s'
        Left True  -> Atomic $ cio io' s'
    (VSignal io' s', VEnum _ 1 [f])  ->
      return $ Left $ VLtl $ case f 0 of
        Right () -> TTrue
        Left False -> Not $ Atomic $ cio io' s'
        Left True  -> Atomic $ cio io' s'
    (VEnum n j _, VSignal _ s') ->
      errBusCmp n s' eqs j 1 pos
    (VSignal _ s', VEnum n j _) ->
      errBusCmp n s' eqs j 1 pos      
    (VEnum {}, _) -> assert False undefined
    (VBus {}, _)  -> assert False undefined
    (_, VEnum {}) -> assert False undefined
    (_, VBus {})  -> assert False undefined    
    _             ->
      if eq a b
      then return $ Right True
      else return $ Right False

  where
    toB _ _  _ a 0 _ = And a
    toB d io s a i f = case f (i-1) of
      Right ()    -> toB d io s a (i-1) f
      Left True  ->
        toB d io s ((Atomic $ cio io $ s ++ d ++ show (i-1)):a) (i-1) f
      Left False ->
        toB d io s ((Not $ Atomic $ cio io $ s ++ d ++ show (i-1)):a) (i-1) f
    
    cio STInput   = Input
    cio STOutput  = Output
    cio STGeneric = assert False undefined

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
  _ -> assert False undefined

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
      A.amap (\x -> if idName x /= idName (t ! bIdent y) then x else x {
                 idBindings =
                    if null $ bVal y 
                    then Expr (SetExplicit []) $ bPos y
                    else Expr (SetExplicit [head $ bVal y]) $ bPos y
                 }) t

-----------------------------------------------------------------------------                   

