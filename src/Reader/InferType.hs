-----------------------------------------------------------------------------
-- |
-- Module      :  Reader.InferType
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
-- 
-- Infers and checks types of all bound expressions.
-- 
-----------------------------------------------------------------------------

module Reader.InferType
    ( inferTypes
    ) where

-----------------------------------------------------------------------------

import Utils
    ( imLookup
    )

import Data.Enum
    ( EnumDefinition(..)
    )  
    
import Data.Types
    ( IdType(..)
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

import Reader.Data
    ( TypeTable
    , ExpressionTable  
    , ArgumentTable
    , Specification(..)  
    )
      
import Reader.Error
    ( Error
    , errExpect
    , errRange
    , errPattern  
    )  

import Data.Maybe
    ( fromJust
    )
    
import Data.Either
    ( partitionEithers
    )
    
import Control.Monad.State
    ( StateT(..)
    , execStateT
    , get
    , put
    , liftM  
    , when  
    )

import Control.Exception
    ( assert
    )  

import qualified Data.IntMap.Strict as IM

-----------------------------------------------------------------------------

type TypeChecker a = a -> StateT ST (Either Error) ()

-----------------------------------------------------------------------------

data ST = ST
  { tCount :: Int
  , tTypes :: TypeTable
  , targs :: ArgumentTable
  }

-----------------------------------------------------------------------------

-- | Infers and checks types of all bound expressions as well as of
-- the assumptions, the invariants and the guarantees. Additionally, a
-- mapping from each identifier to its type is created.

inferTypes
  :: Specification -> Either Error Specification

inferTypes s =
  let
    tt1 = IM.mapWithKey (\i _ -> TPoly i) $ names s
    tt2 = foldl updEnumType tt1 $ enumerations s
    tt3 = foldl (updSignalType STInput) tt2 $ inputs s
    tt4 = foldl (updSignalType STOutput) tt3 $ outputs s
    tt5 = foldl (updType TNumber) tt4 $ map bIdent $ parameters s
    maxkey = 
      if IM.null $ names s then 1 else
        fst $ fst $ fromJust $ IM.maxViewWithKey $ names s
    ts = map fst $ IM.toList tt5
  in do
    tt6 <- execStateT (inferLtl s ts) $ ST (maxkey + 1) tt5 (arguments s)
    return $ s { types = tTypes tt6 }

  where
    updType t a i = IM.insert i t a

    updEnumType a x =
      foldl (updType (TEnum (imLookup (eName x) $ names s) (eName x))) a $
      map (\(y,_,_) -> y) $ eValues x    

    updSignalType t a x = case x of
      SDSingle (y,_)     -> IM.insert y (TSignal t) a
      SDBus (y,_) _      -> IM.insert y (TBus t) a
      SDEnum (y,_) (z,_) ->
        IM.insert y (TTypedBus t (imLookup z $ names s) z) a

-----------------------------------------------------------------------------

inferLtl
  :: Specification -> TypeChecker [Int]

inferLtl s xs = do
  mapM_ inferBusParameter $ inputs s
  mapM_ inferBusParameter $ outputs s    
  let ys = map (\i -> (i,imLookup i $ bindings s)) xs
  mapM_ updateType ys
  mapM_ (inferFromUsage TLtl) $ initially s
  mapM_ (inferFromUsage TLtl) $ preset s
  mapM_ (inferFromUsage TLtl) $ requirements s  
  mapM_ (inferFromUsage TLtl) $ assumptions s
  mapM_ (inferFromUsage TLtl) $ invariants s
  mapM_ (inferFromUsage TLtl) $ guarantees s
  checkTypes (arguments s) (bindings s) ys xs

  where
    inferBusParameter x = case x of
      SDBus _ e -> inferFromUsage TNumber e
      _         -> return ()

-----------------------------------------------------------------------------

checkTypes
  :: ArgumentTable -> ExpressionTable -> [(Int,Expr Int)] -> TypeChecker [Int]

checkTypes as bs ys xs = do
  mapM_ (checkType as) ys
  tt <- get
  let ts = map (flip imLookup $ tTypes tt) xs
      (ns,os) = partitionEithers $ zipWith concrete ts xs
  case ns of
    [] -> return ()
    _  -> inferType as bs os 

  where
    concrete t i = case t of
      TPoly _ -> Right i
      _       -> Left i

-----------------------------------------------------------------------------

inferType
  :: ArgumentTable ->  ExpressionTable -> TypeChecker [Int]

inferType as bs xs = do
  let ys = map (\i -> (i,imLookup i bs)) xs
  mapM_ updateType ys
  checkTypes as bs ys xs

-----------------------------------------------------------------------------

updateType
  :: TypeChecker (Int,Expr Int)

updateType (i,e) = do
  tt <- get
  case imLookup i $ tTypes tt of
    TSignal {}   -> return ()
    TBus {}      -> return ()
    TTypedBus {} -> return ()
    TEnum {}     -> return ()
    _            -> do
      t <- inferFromExpr e
      case t of
        TEmptySet -> return ()
        TSet t'   -> put $ tt {
          tTypes = IM.insert i (generalize t') $ tTypes tt
          }
        TPoly j -> do
          let n = tCount tt
          put $ tt {
            tCount = n + 1,
            tTypes = IM.insert j (TSet $ TPoly n) $ tTypes tt
            }
        _ -> assert False undefined

  where
    generalize t = case t of
      TSignal _ -> TSignal STGeneric
      TSet t'   -> TSet $ generalize t'
      t'        -> t'

-----------------------------------------------------------------------------

checkType
  :: ArgumentTable -> TypeChecker (Int, Expr Int) 

checkType as (i,e) = do
  tt <- get
  case  imLookup i $ tTypes tt of
    TSignal {}   -> return ()
    TEnum {}     -> return ()
    TTypedBus {} -> return ()
    TBus {}      -> inferFromUsage (TSet TNumber) e
    t            ->  case imLookup i as of
      [] -> inferFromUsage (TSet t) e
      _  -> inferFromUsageFormula (TSet t) e

-----------------------------------------------------------------------------
  
inferFromExpr
  :: Expr Int -> StateT ST (Either Error) IdType

inferFromExpr e = case expr e of
  BaseOtherwise    -> return TBoolean
  Pattern _ _      -> return TBoolean
  BaseWild         -> return TPattern
  BaseCon _        -> return TNumber
  NumSMin _        -> return TNumber
  NumSMax _        -> return TNumber
  NumSSize _       -> return TNumber
  NumSizeOf _      -> return TNumber  
  NumPlus _ _      -> return TNumber
  NumMinus _ _     -> return TNumber
  NumMul _ _       -> return TNumber
  NumDiv _ _       -> return TNumber
  NumMod _ _       -> return TNumber
  NumRPlus _ _     -> return TNumber
  NumRMul _ _      -> return TNumber
  BaseTrue         -> return TLtl
  BaseFalse        -> return TLtl
  BaseBus _ _      -> return TLtl  
  BlnEQ _ _        -> return TLtl
  BlnNEQ _ _       -> return TLtl
  BlnGE _ _        -> return TLtl
  BlnGEQ _ _       -> return TLtl
  BlnLE _ _        -> return TLtl
  BlnLEQ _ _       -> return TLtl
  BlnNot _         -> return TLtl
  BlnOr _ _        -> return TLtl
  BlnROr _ _       -> return TLtl
  BlnAnd _ _       -> return TLtl
  BlnRAnd _ _      -> return TLtl
  BlnImpl _ _      -> return TLtl
  BlnElem _ _      -> return TLtl
  BlnEquiv _ _     -> return TLtl
  LtlNext _        -> return TLtl
  LtlRNext _ _     -> return TLtl
  LtlGlobally _    -> return TLtl
  LtlRGlobally _ _ -> return TLtl
  LtlFinally _     -> return TLtl
  LtlRFinally _ _  -> return TLtl
  LtlUntil _ _     -> return TLtl
  LtlWeak _ _      -> return TLtl
  LtlRelease _ _   -> return TLtl
  SetExplicit xs   -> inferSetExplicit xs
  SetRange {}      -> return $ TSet TNumber
  SetCup x y       -> inferSetOp x y
  SetRCup _ x      -> inferFromExpr x 
  SetCap x y       -> inferSetOp x y
  SetRCap _ x      -> inferFromExpr x 
  SetMinus x y     -> inferSetOp x y  
  Colon _ x        -> inferFromExpr x
  BaseId i         -> liftM (imLookup i . tTypes) get
  BaseFml _ i      -> liftM (imLookup i . tTypes) get

  where
    inferSetOp x y = do
      tx <- inferFromExpr x
      case tx of
        TEmptySet ->
          inferFromExpr y
        _         -> 
          return tx

    inferSetExplicit xs = case xs of
      []     -> return TEmptySet
      [x]    -> liftM TSet $ inferFromExpr x
      (x:xr) -> do
        t <- inferFromExpr x
        case t of
          TEmptySet -> inferSetExplicit xr
          t'        -> return $ TSet t'

-----------------------------------------------------------------------------

inferFromUsage
  :: IdType -> TypeChecker (Expr Int)

inferFromUsage t e = case t of
  TPoly j -> do
    t' <- inferFromExpr e
    case t' of
      TPoly j' ->
        when (j /= j') $ do
          tt <- get
          put $ tt {
            tTypes = IM.insert (max j j') (TPoly $ min j j') $ tTypes tt
            }
      _        -> do
        tt <- get
        put $ tt {
          tTypes = IM.insert j t' $ tTypes tt
          }
        inferFromUsage t' e 
  TNumber -> inferFromUsageNum e
  TLtl -> inferFromUsageLtl e
  TBoolean -> inferFromUsageBool e
  TPattern -> inferFromUsagePattern e
  TEmptySet -> inferFromUsageEmptySet e
  TSet t' -> inferFromUsageSet t' e
  TSignal _ -> inferFromUsageSignal e
  TBus _ -> inferFromUsageBus e
  TTypedBus _ s i -> inferFromUsageTypedBus s i e
  TEnum s i -> inferFromUsageEnum s i e



-----------------------------------------------------------------------------

inferFromUsageFormula
  :: IdType -> TypeChecker (Expr Int)

inferFromUsageFormula t e = case expr e of
  Colon x y -> do
    inferFromUsage TBoolean x
    inferFromUsage t y
  _         ->
    inferFromUsage t e

-----------------------------------------------------------------------------

inferFromUsageNum
  :: TypeChecker (Expr Int)

inferFromUsageNum e = case expr e of
  BaseCon _    -> return ()
  NumSMin x    -> inferFromUsage (TSet TNumber) x
  NumSMax x    -> inferFromUsage (TSet TNumber) x
  NumSizeOf x  -> inferFromUsage (TBus STGeneric) x
  NumSSize x   -> do
    t <- inferFromExpr x
    case t of
      TPoly j   -> do
        tt <- get
        let n = tCount tt
        put $ tt {
          tCount = n + 1,
          tTypes = IM.insert j (TSet $ TPoly n) $ tTypes tt
          }
        inferFromUsage (TSet $ TPoly n) x
      TEmptySet -> return ()
      TSet y    -> inferFromUsage (TSet y) x
      y         -> errExpect (TSet (TPoly (-1))) y $ srcPos e
  NumPlus x y  -> mapM_ (inferFromUsage TNumber) [x,y]
  NumMinus x y -> mapM_ (inferFromUsage TNumber) [x,y]
  NumMul x y   -> mapM_ (inferFromUsage TNumber) [x,y]
  NumDiv x y   -> mapM_ (inferFromUsage TNumber) [x,y]
  NumMod x y   -> mapM_ (inferFromUsage TNumber) [x,y]
  NumRPlus _ x -> inferFromUsage TNumber x
  NumRMul _ x  -> inferFromUsage TNumber x
  BaseId i     -> inferFromUsageId TNumber (srcPos e) i
  BaseFml as i -> inferFromUsageFml as TNumber (srcPos e) i
  Colon x y    -> do
    inferFromUsage TBoolean x
    inferFromUsage TNumber y  
  _            -> do
    t <- inferFromExpr e
    errExpect TNumber t $ srcPos e

-----------------------------------------------------------------------------

inferFromUsageBool
  :: TypeChecker (Expr Int)

inferFromUsageBool e = case expr e of
  BaseOtherwise -> return ()
  BaseTrue      -> return ()
  BaseFalse     -> return ()
  Pattern z v   -> do
    inferFromUsage TLtl z
    inferFromUsage TPattern v
  BlnEQ x y     -> inferFromEquality x y
  BlnNEQ x y    -> inferFromEquality x y 
  BlnGE x y     -> mapM_ (inferFromUsage TNumber) [x,y]
  BlnGEQ x y    -> mapM_ (inferFromUsage TNumber) [x,y]
  BlnLE x y     -> mapM_ (inferFromUsage TNumber) [x,y]
  BlnLEQ x y    -> mapM_ (inferFromUsage TNumber) [x,y]
  BlnElem x y   -> do
    t <- inferFromExpr x
    inferFromUsage t x
    inferFromUsage (TSet t) y
  BlnNot x      -> inferFromUsage TBoolean x
  BlnOr x y     -> mapM_ (inferFromUsage TBoolean) [x,y]
  BlnAnd x y    -> mapM_ (inferFromUsage TBoolean) [x,y]
  BlnImpl x y   -> mapM_ (inferFromUsage TBoolean) [x,y]
  BlnEquiv x y  -> mapM_ (inferFromUsage TBoolean) [x,y]
  BlnROr _ x    -> inferFromUsage TBoolean x
  BlnRAnd _ x   -> inferFromUsage TBoolean x
  Colon x y     -> do
    inferFromUsage TBoolean x
    inferFromUsage TBoolean y    
  _             -> do
    t <- inferFromExpr e
    errExpect TBoolean t $ srcPos e

-----------------------------------------------------------------------------

inferFromUsageLtl
  :: TypeChecker (Expr Int)

inferFromUsageLtl e = case expr e of
  BaseTrue           -> return ()
  BaseFalse          -> return ()
  BaseBus x i        -> do
    inferFromUsage TNumber x
    inferFromUsageId (TBus STGeneric) (srcPos e) i
  BlnEQ x y        -> inferFromEquality x y
  BlnNEQ x y       -> inferFromEquality x y
  BlnGE x y        -> mapM_ (inferFromUsage TNumber) [x,y]
  BlnGEQ x y       -> mapM_ (inferFromUsage TNumber) [x,y]
  BlnLE x y        -> mapM_ (inferFromUsage TNumber) [x,y]
  BlnLEQ x y       -> mapM_ (inferFromUsage TNumber) [x,y]
  BlnElem _ _      -> inferFromUsage TBoolean e
  BlnNot x         -> inferFromUsage TLtl x
  BlnOr x y        -> mapM_ (inferFromUsage TLtl) [x,y]
  BlnAnd x y       -> mapM_ (inferFromUsage TLtl) [x,y]
  BlnImpl x y      -> mapM_ (inferFromUsage TLtl) [x,y]
  BlnEquiv x y     -> mapM_ (inferFromUsage TLtl) [x,y]
  BlnROr _ x       -> inferFromUsage TLtl x
  BlnRAnd _ x      -> inferFromUsage TLtl x
  LtlNext x        -> inferFromUsage TLtl x
  LtlGlobally x    -> inferFromUsage TLtl x
  LtlFinally x     -> inferFromUsage TLtl x  
  LtlUntil x y     -> mapM_ (inferFromUsage TLtl) [x,y]
  LtlWeak x y      -> mapM_ (inferFromUsage TLtl) [x,y]
  LtlRelease x y   -> mapM_ (inferFromUsage TLtl) [x,y]
  LtlRNext x y     -> do
    inferFromUsage TNumber x
    inferFromUsage TLtl y
  LtlRGlobally x y -> do
    inferFromUsageRange x
    inferFromUsage TLtl y
  LtlRFinally x y  -> do
    inferFromUsageRange x
    inferFromUsage TLtl y
  BaseId i         -> inferFromUsageId TLtl (srcPos e) i
  BaseFml as i     -> inferFromUsageFml as TLtl (srcPos e) i
  Colon x y        -> do
    inferFromUsage TBoolean x
    inferFromUsage TLtl y
  _                -> do
    t <- inferFromExpr e
    errExpect TLtl t $ srcPos e

-----------------------------------------------------------------------------

inferFromEquality
  :: Expr Int -> TypeChecker (Expr Int)

inferFromEquality x y = do
  t <- inferFromExpr x
  mapM_ (inferFromUsage t) [x,y]
  tt <- get
  case checkForEnum tt x y of
    Nothing      -> mapM_ (inferFromUsage t) [x,y]
    Just (s,i,z) -> inferFromUsage (TTypedBus STGeneric s i) z

  where
    checkForEnum tt a b = case expr a of
      BaseId i -> case imLookup i $ tTypes tt of
        TEnum s t -> Just (s,t,b)
        _         -> checkForEnum' tt b a
      _ -> checkForEnum' tt b a

    checkForEnum' tt a b = case expr a of
      BaseId i ->  case imLookup i $ tTypes tt of
        TEnum s t -> Just (s,t,b)
        _         -> Nothing
      _ -> Nothing      
        
-----------------------------------------------------------------------------

inferFromUsagePattern
  :: TypeChecker (Expr Int)

inferFromUsagePattern e = case expr e of
  BaseWild         -> return ()
  BaseTrue         -> return ()
  BaseFalse        -> return ()
  BlnNot x         -> inferFromUsage TPattern x
  BlnOr x y        -> mapM_ (inferFromUsage TPattern) [x,y]
  BlnAnd x y       -> mapM_ (inferFromUsage TPattern) [x,y]
  BlnImpl x y      -> mapM_ (inferFromUsage TPattern) [x,y]
  BlnEquiv x y     -> mapM_ (inferFromUsage TPattern) [x,y]
  LtlNext x        -> inferFromUsage TPattern x
  LtlGlobally x    -> inferFromUsage TPattern x
  LtlFinally x     -> inferFromUsage TPattern x  
  LtlUntil x y     -> mapM_ (inferFromUsage TPattern) [x,y]
  LtlWeak x y      -> mapM_ (inferFromUsage TPattern) [x,y]
  LtlRelease x y   -> mapM_ (inferFromUsage TPattern) [x,y]
  LtlRNext x y     -> inferFromUsage TNumber x >> inferFromUsage TPattern y
  LtlRGlobally x y -> inferFromUsageRange x >> inferFromUsage TPattern y
  LtlRFinally x y  -> inferFromUsageRange x >> inferFromUsage TPattern y
  BaseId i         -> inferFromUsageId TLtl (srcPos e) i
  _                -> errPattern $ srcPos e

-----------------------------------------------------------------------------

inferFromUsageEmptySet
  :: TypeChecker (Expr Int)

inferFromUsageEmptySet e = case expr e of
  SetExplicit _ -> return ()
  SetRange {}   -> return ()
  SetCup x y    -> mapM_ (inferFromUsage TEmptySet) [x,y]
  SetCap x y    -> mapM_ (inferFromUsage TEmptySet) [x,y]
  SetMinus x y  -> mapM_ (inferFromUsage TEmptySet) [x,y]
  SetRCup _ x   -> inferFromUsage TEmptySet x
  SetRCap _ x   -> inferFromUsage TEmptySet x
  _             -> do
    t <- inferFromExpr e
    errExpect (TSet (TPoly (-1))) t $ srcPos e

-----------------------------------------------------------------------------

inferFromUsageSet
  :: IdType -> TypeChecker (Expr Int)

inferFromUsageSet t e = case expr e of
  SetExplicit xs -> mapM_ (inferFromUsage t) xs
  SetRange x y z -> mapM_ (inferFromUsage t) [x,y,z]
  SetCup x y     -> mapM_ (inferFromUsage (TSet t)) [x,y]
  SetCap x y     -> mapM_ (inferFromUsage (TSet t)) [x,y]
  SetMinus x y   -> mapM_ (inferFromUsage (TSet t)) [x,y]
  SetRCup _ x    -> inferFromUsage (TSet t) x
  SetRCap _ x    -> inferFromUsage (TSet t) x
  BaseId i       -> inferFromUsageId (TSet t) (srcPos e) i
  BaseFml as i   -> inferFromUsageFml as (TSet t) (srcPos e) i
  Colon x y      -> do
    inferFromUsage TBoolean x
    inferFromUsage (TSet t) y  
  _              -> do
    t' <- inferFromExpr e
    errExpect (TSet t) t' $ srcPos e

-----------------------------------------------------------------------------

inferFromUsageSignal
  :: TypeChecker (Expr Int)

inferFromUsageSignal e = case expr e of
  BaseId i     -> inferFromUsageId (TSignal STGeneric) (srcPos e) i
  BaseFml as i -> inferFromUsageFml as (TSignal STGeneric) (srcPos e) i
  _            -> do
    t <- inferFromExpr e
    errExpect (TSignal STGeneric) t $ srcPos e

-----------------------------------------------------------------------------

inferFromUsageBus
  :: TypeChecker (Expr Int)

inferFromUsageBus e = case expr e of
  BaseId i     -> inferFromUsageId (TBus STGeneric) (srcPos e) i
  BaseFml as i -> inferFromUsageFml as (TBus STGeneric) (srcPos e) i  
  _            -> do
    t <- inferFromExpr e
    errExpect (TBus STGeneric) t $ srcPos e

-----------------------------------------------------------------------------

inferFromUsageTypedBus
  :: String -> Int -> TypeChecker (Expr Int)

inferFromUsageTypedBus s y e = case expr e of
  BaseId i     -> inferFromUsageId (TTypedBus STGeneric s y) (srcPos e) i
  BaseFml as i -> inferFromUsageFml as (TTypedBus STGeneric s y) (srcPos e) i    
  _            -> do
    t <- inferFromExpr e
    errExpect (TTypedBus STGeneric s y) t $ srcPos e

-----------------------------------------------------------------------------

inferFromUsageEnum
  :: String -> Int -> TypeChecker (Expr Int)

inferFromUsageEnum s y e = case expr e of
  BaseId i     -> inferFromUsageId (TEnum s y) (srcPos e) i
  BaseFml as i -> inferFromUsageFml as (TEnum s y) (srcPos e) i      
  _            -> do
    t <- inferFromExpr e
    errExpect (TEnum s y) t $ srcPos e    

-----------------------------------------------------------------------------    

inferFromUsageRange
  :: TypeChecker (Expr Int)

inferFromUsageRange e = case expr e of
  Colon x y -> mapM_ (inferFromUsage TNumber) [x,y]
  _         -> do
    t <- inferFromExpr e
    errRange t $ srcPos e

---    

inferFromUsageId
  :: IdType -> ExprPos -> TypeChecker Int

inferFromUsageId t pos i = do
  tt <- get
  let t' = imLookup i $ tTypes tt
  updIdType id t t' 

  where
    updIdType c t1 t2 = case (t1,t2) of
      (TPoly a, TPoly b) -> when (a /= b) $ do
        tt <- get
        put $ tt {
          tTypes = IM.insert (min a b) (c $ TPoly $ max a b) $ tTypes tt
          }
      (TPoly a, TSignal _) -> do
        tt <- get
        put $ tt {
          tTypes = IM.insert a (c $ TSignal STGeneric) $ tTypes tt
          }
      (TPoly a, TBus _) -> do
        tt <- get
        put $ tt {
          tTypes = IM.insert a (c $ TBus STGeneric) $ tTypes tt
          }
      (TPoly a, TTypedBus _ x y) -> do
        tt <- get
        put $ tt {
          tTypes = IM.insert a (c $ TTypedBus STGeneric x y) $ tTypes tt
          }
      (TPoly a, _)       -> do
        tt <- get
        put $ tt {
          tTypes = IM.insert a (c t2) $ tTypes tt
          }
      (_, TPoly b)       -> do
        tt <- get
        put $ tt {
          tTypes = IM.insert i (c t1) $ IM.insert b (c t1) $ tTypes tt
          }
      (TBus {}, TEnum {})          -> return ()
      (TBus {}, TTypedBus {})      -> return ()
      (TBus {}, TSignal {})        -> return ()            
      (TBus {}, TBus {})           -> return ()
      (TTypedBus {}, TBus {})      -> return ()
      (TTypedBus {}, TEnum {})     -> return ()
      (TTypedBus {}, TSignal {})   -> return ()            
      (TTypedBus {}, TTypedBus {}) -> return ()      
      (TEnum {}, TBus {})          -> return ()      
      (TEnum {}, TTypedBus {})     -> return ()
      (TEnum {}, TSignal {})       -> return ()
      (TEnum {}, TEnum {})         -> return ()            

      (TSet a, TSet b)   ->
        updIdType (c . TSet) a b 
      _                  -> 
        when (t1 /= t2 && ((t1 /= TLtl && noSignal t1) || noSignal t2)) $
          errExpect t1 t2 pos

    noSignal x = case x of
      TSignal _ -> False
      _         -> True

-----------------------------------------------------------------------------

inferFromUsageFml
  :: [Expr Int] -> IdType -> ExprPos -> TypeChecker Int

inferFromUsageFml as t pos i = do
  inferFromUsageId t pos i
  st <- get
  let xs = zip as $ map (\x -> imLookup x $ tTypes st) $ imLookup i $ targs st
  mapM_ (\(x,y) -> inferFromUsage y x) xs

-----------------------------------------------------------------------------
