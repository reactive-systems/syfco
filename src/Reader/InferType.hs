-----------------------------------------------------------------------------
-- |
-- Module      :  Reader.InferType
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- Infers and checks types of all bound expressions.
--
-----------------------------------------------------------------------------

{- TODO NOTES:

 * rewrite type check

 ** two phases: genereric deriving; derive types only from the
    expression structure. However, do not use the embedding to derive
    concrete types. Then, in the second phase: check types according to
    their usage

 * first produce typecheck procedure. With a complete typetable at
   hand, type checking will be straightforward.

 * type inference:

 ** get list of all top level expresions ordered by their dependencies.

 ** derive the type of the subexpression: for every function
    application safe state before, then derive subtypes. If the type is
    derived, typecheck the subexpression. After returning from functions,
    restore their original type which is saved in the call strack during
    the recursive decension.

-}

-----------------------------------------------------------------------------

{-# LANGUAGE

    LambdaCase
  , TupleSections
  , RecordWildCards
  , MultiWayIf
  , ViewPatterns

  #-}

-----------------------------------------------------------------------------

module Reader.InferType
  ( inferTypes
  ) where

-----------------------------------------------------------------------------

import Prelude hiding ((!))

import Control.Arrow
  ( (>>>)
  )

import Control.Monad
  ( foldM
  )

import Utils
  ( imLookup
  )

import Data.Graph
  ( Graph
  , buildG
  , transposeG
  , topSort
  )

import qualified Data.Set as S
  ( fromList
  , toList
  , insert
  , intersection
  , difference
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
  ( Binding
  , BindExpr(..)
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
  , errNoPFuns
  , errArgArity
  , errNoHigher
  , errEquality
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

import Data.IntMap.Strict
  ( (!)
  )

import qualified Data.IntMap.Strict as IM
  ( insert
  , null
  , fromList
  , map
  , empty
  , toList
  , mapWithKey
  , maxViewWithKey
  , findMax
  )

import  Data.IntMap.Strict
  ( IntMap
  )

import Debug.Trace

-----------------------------------------------------------------------------

type TypeChecker a = a -> StateT ST (Either Error) ()

type TC a = StateT ST (Either Error) a

type ExprType = IdType

type ID = Int

type Expression = Expr ID

-----------------------------------------------------------------------------

data ST = ST
  { tCount :: Int
  , tTypes :: TypeTable
  , targs :: ArgumentTable
  , spec :: Specification
  , curBinding :: ID
  }

-----------------------------------------------------------------------------

-- | Infers and checks types of all bound expressions as well as of
-- the assumptions, the invariants and the guarantees. Additionally, a
-- mapping from each identifier to its type is created.

inferTypes
  :: Specification -> Either Error Specification

inferTypes s@Specification{..} = do
  let st = ST { tCount = (+1) $ fst $ IM.findMax names
              , tTypes = IM.mapWithKey (flip $ const TPoly) names
              , targs = arguments
              , spec = s
              , curBinding = -1
              }

  tt <- execStateT inferTypeSpec st
  return $ s { types = tTypes tt }

{-  let
    -- assign arbitrary type to each element
    tt1 :: TypeTable
    tt1 = IM.mapWithKey (\i _ -> TPoly i) names

    -- set enum types
    tt2 = foldl updEnumType tt1 enumerations
    -- set the signal types
    tt3 = foldl (updSignalType STInput) tt2 inputs
    tt4 = foldl (updSignalType STOutput) tt3 outputs
    -- set the parameter types
    tt5 = foldl (updType TNumber) tt4 $ map bIdent parameters
    -- get the maximal count
    maxkey =
      if IM.null $ names then 1 else
        fst $ fst $ fromJust $ IM.maxViewWithKey names
    -- constuct preliminary type table
    ts = map fst $ IM.toList tt5
  in do
    --




    -- construct updated type table
--    tt6 <- execStateT (inferLtl s ts) $ ST (maxkey + 1) tt5 (arguments s)
--    return $ s { types = tTypes tt6 }

  where
    updType t a i = IM.insert i t a

    updEnumType a x =
      foldl (updType (TEnum (imLookup (eName x) names) (eName x))) a $
      map (\(y,_,_) -> y) $ eValues x

    updSignalType t a x = case x of
      SDSingle (y,_)     -> IM.insert y (TSignal t) a
      SDBus (y,_) _      -> IM.insert y (TBus t) a
      SDEnum (y,_) (z,_) ->
        IM.insert y (TTypedBus t (imLookup z names) z) a -}

-----------------------------------------------------------------------------

-- | Returns the list of bindings, which is ordered accoring to the
-- respective dependencies of the entries. The result is a list of
-- @Either@s, where @Left@ entries corresponds to parameter bindings
-- and @Right@ entries to definition bindings.

depOrderedBindings
  :: Specification -> [Either (BindExpr ID) (BindExpr ID)]

depOrderedBindings Specification{..} =
  let
    -- get ID map
    im =
      IM.fromList
        $ (map (\x -> (bIdent x, Left x)) parameters) ++
          (map (\x -> (bIdent x, Right x)) definitions)

    -- get the parameter ids
    ps :: [ID]
    ps = map bIdent parameters

    -- get the definition ids
    ds :: [ID]
    ds = map bIdent definitions

    -- list of all bindings ids
    xs :: [ID]
    xs = ps ++ ds
  in
    if null xs
    then []
    else
      let
        -- create list of edges
        es :: [(ID, ID)]
        es = concatMap (\x -> map (x,) $ deps xs x) xs

        -- create the depencency graph
        g :: Graph
        g = buildG (minimum xs, maximum xs) es
      in
        map (im !) $
        filter (`elem` xs) $
        topSort $
        transposeG g

  where
    deps
      :: [Int] -> Int -> [Int]

    deps xs x =
      S.toList $
      S.intersection
        (S.fromList $ dependencies ! x)
        (S.fromList xs)

-----------------------------------------------------------------------------

inferTypeSpec
  :: TC ()

inferTypeSpec = do
  -- get the specification
  s@Specification{..} <- liftM spec get

  -- set the enumeration types
  mapM_ setEnumType enumerations
  -- set the signal types fo input and outputs
  mapM_ (setSignalType STInput) inputs
  mapM_ (setSignalType STOutput) outputs
  -- set the parameter types
  mapM_ (updType TNumber) $ map bIdent parameters

  -- fix and check the binding types
  mapM_ typeCheckBinding $ depOrderedBindings s
  -- type-check bus-parameters
  mapM_ typeCheckBusParameter inputs
  mapM_ typeCheckBusParameter outputs
  -- type-check LTL formulas
  mapM_ (flip typeCheck TLtl) initially
  mapM_ (flip typeCheck TLtl) preset
  mapM_ (flip typeCheck TLtl) requirements
  mapM_ (flip typeCheck TLtl) assumptions
  mapM_ (flip typeCheck TLtl) invariants
  mapM_ (flip typeCheck TLtl) guarantees

  where
    setSignalType
      :: SignalType -> SignalDecType ID -> TC ()

    setSignalType s = \case
      SDSingle (x,_)     -> updType (TSignal s) x
      SDBus (x,_) _      -> updType (TBus s) x
      SDEnum (x,_) (y,_) -> do
        Specification{..} <- liftM spec get
        updType (TTypedBus s (imLookup y names) y) x


    setEnumType
      :: EnumDefinition ID -> TC ()

    setEnumType e = do
      Specification{..} <- liftM spec get

      mapM_
        (updType (TEnum (imLookup (eName e) names) $ eName e))
        (map (\(y,_,_) -> y) $ eValues e)

-----------------------------------------------------------------------------

typeCheckBinding
  :: Either (BindExpr ID) (BindExpr ID) -> TC ()

typeCheckBinding = \case
  Left x  -> typeCheckParameter $ bIdent x
  Right x -> typeCheckDefinition $ bIdent x

-----------------------------------------------------------------------------

typeCheckParameter
  :: ID -> TC ()

typeCheckParameter i = do
  Specification{..} <- liftM spec get

  let
    n :: Int
    n = length $ arguments ! i

  if n <= 0
  then typeCheck (bindings ! i) $ TSet TNumber
  else errNoPFuns n $ positions ! i

-----------------------------------------------------------------------------

typeCheckDefinition
  :: ID -> TC ()

typeCheckDefinition i = do
  -- set the current binding to detect recursive function definitions
  setCur i

  -- get the expression bound the the identifier
  e <- liftM ((! i) . bindings . spec) get

  -- get the type of the expression
  TSet t <- inferFromExpr e >>= \case
    TSet TBoolean -> inferFromBoolExpr e
    t'            -> return t'

  -- update the type in the type table
  updType t i
  -- typecheck the expression
  typeCheck e $ TSet t
  -- reset the recursion indicator
  setCur (-1)

  where
    setCur
      :: Int -> TC ()

    setCur c = do
      st <- get
      put st { curBinding = c }

-----------------------------------------------------------------------------

typeCheckBusParameter
  :: SignalDecType ID -> TC ()

typeCheckBusParameter = \case
  SDBus _ e -> typeCheck e TNumber
  _         -> return ()

-----------------------------------------------------------------------------

typeCheck
  :: Expression -> ExprType -> TC ()

typeCheck e = \case
  -- check signal types
  TSignal x           -> typeChIdF e $ TSignal x

  -- check normal bus types
  TBus x              -> typeChIdF e $ TBus x

  -- check enum bus types
  TEnum t x           -> typeChIdF e $ TEnum t x

  -- check typed bus types
  TTypedBus x y z     -> typeChIdF e $ TTypedBus x y z

  -- check for empty sets
  TEmptySet -> case expr e of
    SetExplicit []    -> return ()
    SetExplicit (x:_) -> typeErrES e x
    SetCup x y        -> typeChck2 x y TEmptySet
    SetCap x y        -> typeChck2 x y TEmptySet
    SetMinus x y      -> typeChck2 x y TEmptySet
    SetRCup _ x       -> typeCheck x TEmptySet
    SetRCap _ x       -> typeCheck x TEmptySet
    _                 -> typeChIdF e TEmptySet

  -- check set types
  TSet t -> case expr e of
    SetExplicit xs    -> typeChSEx t xs
    SetRange x y z    -> typeChSRg x y z
    SetCup x y        -> typeChck2 x y $ TSet t
    SetRCup xs x      -> typeChckO xs x $ TSet t
    SetCap x y        -> typeChck2 x y $ TSet t
    SetRCap xs x      -> typeChckO xs x $ TSet t
    SetMinus x y      -> typeChck2 x y $ TSet t
    _                 -> typeChIdF e $ TSet t

  -- check numerical types
  TNumber -> case expr e of
    BaseCon {}        -> return ()
    NumSMin xs        -> typeCheck xs $ TSet TNumber
    NumSMax xs        -> typeCheck xs $ TSet TNumber
    NumSSize x        -> typeChckS x
    NumSizeOf b       -> typeCheck b $ TBus STGeneric
    NumPlus x y       -> typeChck2 x y TNumber
    NumRPlus xs x     -> typeChckO xs x TNumber
    NumMinus x y      -> typeChck2 x y TNumber
    NumMul x y        -> typeChck2 x y TNumber
    NumRMul xs x      -> typeChckO xs x TNumber
    NumDiv x y        -> typeChck2 x y TNumber
    NumMod x y        -> typeChck2 x y TNumber
    _                 -> typeChIdF e TNumber

  -- check boolean types
  TBoolean -> case expr e of
    BaseTrue          -> return ()
    BaseFalse         -> return ()
    BaseOtherwise     -> return ()
    Pattern x y       -> typeChckP e
    BlnElem x xs      -> typeChElm x xs
    BlnEQ x y         -> typeChEqB x y
    BlnNEQ x y        -> typeChEqB x y
    BlnGE x y         -> typeChck2 x y TNumber
    BlnGEQ x y        -> typeChck2 x y TNumber
    BlnLE x y         -> typeChck2 x y TNumber
    BlnLEQ x y        -> typeChck2 x y TNumber
    BlnNot x          -> typeCheck x TBoolean
    BlnOr x y         -> typeChck2 x y TBoolean
    BlnAnd x y        -> typeChck2 x y TBoolean
    BlnImpl x y       -> typeChck2 x y TBoolean
    BlnEquiv x y      -> typeChck2 x y TBoolean
    BlnROr xs x       -> typeChckO xs x TBoolean
    BlnRAnd xs x      -> typeChckO xs x TBoolean
    _                 -> typeChIdF e TBoolean

  -- check LTL formula types
  TLtl -> case expr e of
    BaseTrue          -> return ()
    BaseFalse         -> return ()
    BlnElem x xs      -> typeChElm x xs
    BlnEQ x y         -> typeChEqL x y
    BlnNEQ x y        -> typeChEqL x y
    BlnGE x y         -> typeChck2 x y TNumber
    BlnGEQ x y        -> typeChck2 x y TNumber
    BlnLE x y         -> typeChck2 x y TNumber
    BlnLEQ x y        -> typeChck2 x y TNumber
    BlnNot x          -> typeCheck x TLtl
    BlnOr x y         -> typeChck2 x y TLtl
    BlnAnd x y        -> typeChck2 x y TLtl
    BlnImpl x y       -> typeChck2 x y TLtl
    BlnEquiv x y      -> typeChck2 x y TLtl
    BlnROr xs x       -> typeChckO xs x TLtl
    BlnRAnd xs x      -> typeChckO xs x TLtl
    LtlNext x         -> typeCheck x TLtl
    LtlRNext x y      -> typeChckR x y
    LtlGlobally x     -> typeCheck x TLtl
    LtlRGlobally x y  -> typeChckR x y
    LtlFinally x      -> typeCheck x TLtl
    LtlRFinally x y   -> typeChckR x y
    LtlUntil x y      -> typeChck2 x y TLtl
    LtlWeak x y       -> typeChck2 x y TLtl
    LtlRelease x y    -> typeChck2 x y TLtl
    _                 -> typeChIdF e TLtl

  TPattern -> case expr e of
    BaseTrue          -> return ()
    BaseFalse         -> return ()
    BaseWild          -> return ()
    BlnElem x xs      -> typeChElm x xs
    BlnEQ x y         -> typeChck2 x y TNumber
    BlnNEQ x y        -> typeChck2 x y TNumber
    BlnGE x y         -> typeChck2 x y TNumber
    BlnGEQ x y        -> typeChck2 x y TNumber
    BlnLE x y         -> typeChck2 x y TNumber
    BlnLEQ x y        -> typeChck2 x y TNumber
    BlnNot x          -> typeCheck x TPattern
    BlnOr x y         -> typeChck2 x y TPattern
    BlnAnd x y        -> typeChck2 x y TPattern
    BlnImpl x y       -> typeChck2 x y TPattern
    BlnEquiv x y      -> typeChck2 x y TPattern
    BlnROr xs x       -> typeChckO xs x TPattern
    BlnRAnd xs x      -> typeChckO xs x TPattern
    LtlNext x         -> typeCheck x TPattern
    LtlRNext x y      -> typeChckU x y
    LtlGlobally x     -> typeCheck x TPattern
    LtlRGlobally x y  -> typeChckU x y
    LtlFinally x      -> typeCheck x TPattern
    LtlRFinally x y   -> typeChckU x y
    LtlUntil x y      -> typeChck2 x y TPattern
    LtlWeak x y       -> typeChck2 x y TPattern
    LtlRelease x y    -> typeChck2 x y TPattern
    _                 -> typeChIdF e TPattern

  TPoly i -> inferFromExpr e >>= \case
    TPoly j
      | i == j     -> typeChIdF e $ TPoly i
      | otherwise -> equalPolyType i j >> inferFromExpr e >>= typeCheck e
    t             -> updatePolyType i t >> typeCheck e t

-----------------------------------------------------------------------------

typeChIdF
  :: Expression -> ExprType -> TC ()

typeChIdF e t = case expr e of
  BaseId i     -> typeCheckId t i $ srcPos e
  BaseFml xs f -> typeCheckFml t xs f $ srcPos e
  BaseBus x b  -> typeCheckBus t x b $ srcPos e
  Colon x y    -> typeCheck x TBoolean >> typeCheck y t
  x            -> do
    TSet t' <- inferFromExpr e
    errExpect t t' $ srcPos e

-----------------------------------------------------------------------------

typeCheckBus
  :: ExprType -> Expression -> ID -> ExprPos -> TC ()

typeCheckBus t n b p = do
  typeCheck n TNumber

  liftM ((! b) . tTypes) get >>= \case
    TBus s'          -> case t of
      TSignal s -> case (s,s') of
        (STInput, STOutput) -> errExpect t (TSignal s') p
        (STOutput, STInput) -> errExpect t (TSignal s') p
        _                   -> return ()
      TLtl      -> return ()
      _         -> errExpect t (TSignal s') p
    TTypedBus s' _ _ -> case t of
      TSignal s -> case (s,s') of
        (STInput, STOutput) -> errExpect t (TSignal s') p
        (STOutput, STInput) -> errExpect t (TSignal s') p
        _                   -> return ()
      TLtl      -> return ()
      _         -> errExpect t (TSignal s') p
    TPoly p          -> updatePolyType p $ TBus STGeneric
    t'               -> errExpect t t' p

-----------------------------------------------------------------------------

typeCheckId
  :: ExprType -> Int -> ExprPos -> TC ()

typeCheckId t i p = do
  -- get the arguments of the identifier
  as <- liftM ((! i) . arguments . spec) get

  -- higher order functions are not supported
  when (not $ null as) $ do
    Specification{..} <- liftM spec get
    errNoHigher (names ! i) p

  tt <- liftM tTypes get
  validTypes p i t (tt ! i)

-----------------------------------------------------------------------------

validTypes
  :: ExprPos -> Int -> ExprType -> ExprType
      -> StateT ST (Either Error) ()

validTypes p i = vt
  where
    vt (TSignal STGeneric) (TSignal _)         = return ()
    vt (TSignal _)         (TSignal STGeneric) = return ()
    vt (TBus STGeneric)    (TBus _)            = return ()
    vt (TBus _)            (TBus STGeneric)    = return ()
    vt (TEnum m x)         (TTypedBus _ n y)   = vt (TEnum m x) $ TEnum n y
    vt (TTypedBus _ m x)   (TEnum n y)         = vt (TEnum m x) $ TEnum n y
    vt (TTypedBus _ m x)   (TTypedBus _ n y)   = vt (TEnum m x) $ TEnum n y
    vt (TEmptySet)         (TSet _)            = return ()
    vt (TSet s)            (TEmptySet)         = updType (TSet s) i
    vt (TSet s)            (TSet s')           = vt s s'
    vt (TLtl)              (TBoolean)          = return ()
    vt (TLtl)              (TSignal _)         = return ()
    vt (TPoly p')          (TPoly p)           = equalPolyType p p'
    vt (TPoly _)           _                   = return ()
    vt t                   (TPoly p)           = updatePolyType p t
    vt t                   t'
      | t == t'    = return ()
      | otherwise = errExpect t t' p

-----------------------------------------------------------------------------

typeCheckFml
  :: ExprType -> [Expression] -> Int -> ExprPos -> TC ()

typeCheckFml t xs f p = do
  -- get the position of the definition of f
  pf <- liftM ((! f) . positions . spec) get
  -- get athe rguments of f
  as <- liftM ((! f) . arguments . spec) get
  -- get current type table
  tt <- liftM tTypes get
  -- get argument types of f
  let at = map (tt !) as

  -- check argument arity
  when (length as /= length xs) $
    errArgArity (show $ length xs) (length as) pf p

  -- typecheck the arguments
  mapM_ (uncurry typeCheck) $ zip xs at
  -- get the result type
  rt <- liftM ((! f) . tTypes) get
  -- reset bound types for dependant functions (polymorphism)
  resetDependencies tt f
  -- check the compatibility of the result type
  validTypes p f t rt



  -- check argument expression types
--  mapM_ typeCheckArg xs
  -- get argument expression types
--  ts <- mapM inferFromExpr xs
  -- save current type table
--  tt <- liftM tTypes get


  -- instantiate arguments with argument types
--  mapM_ (uncurry updType) $ zip as ts



  -- restore argument types
--  mapM_ (resetType tt) as



--  when (rt /= t) $
--    error "x" --errExpect t rt $ srcPos e

  -- TODO: check expression building overflow
  -- f(x) = { f(x) }

-----------------------------------------------------------------------------

resetDependencies
  :: TypeTable -> ID -> TC ()

resetDependencies tt x = do
  i <- liftM curBinding get
  as <- liftM (S.fromList . (! x) . arguments . spec) get
  ds <- liftM (S.fromList . (! x) . dependencies . spec) get

  if i == x
  then
    mapM_ (resetDependencies tt) $
    S.toList $
    S.difference ds $
    S.insert x as
  else do
    resetType tt x
    mapM_ (resetDependencies tt) $
      filter (/= x) $ S.toList ds

-----------------------------------------------------------------------------

resetType
  :: TypeTable -> Int -> TC ()

resetType tt i = do
  st@ST{..} <- get
  put st { tTypes = IM.insert i (tt ! i) tTypes }

-----------------------------------------------------------------------------

updType
  :: ExprType -> ID -> TC ()

updType t i = do
  st@ST{..} <- get
  put st { tTypes = IM.insert i t tTypes }

-----------------------------------------------------------------------------

{-typeCheckArg
  :: Expression -> TC ()

typeCheckArg e =
  inferFromExpr e >>= typeCheck e
-}

-----------------------------------------------------------------------------

equalPolyType
  :: Int -> Int -> TC ()

equalPolyType n m = do
  st@ST{..} <- get
  put st { tTypes = IM.map upd tTypes }

  where
    upd = \case
      TPoly x
        | x == n || x == m -> TPoly $ min n m
        | otherwise     -> TPoly x
      x                 -> x

-----------------------------------------------------------------------------

updatePolyType
  :: Int -> ExprType -> TC ()

updatePolyType n t = do
  st@ST{..} <- get
  put st { tTypes = IM.map upd tTypes }

  where
    upd = \case
      TPoly x
        | x == n     -> t
        | otherwise -> TPoly x
      x             -> x

-----------------------------------------------------------------------------

typeChEqL
  :: Expression -> Expression -> TC ()

typeChEqL x y = inferFromExpr x >>= \case
  TNumber -> typeChck2 x y TNumber
  TTypedBus s m i -> inferFromExpr y >>= \case
    TEnum n j
      | i == j     -> return ()
      | otherwise -> errExpect (TEnum m i) (TEnum n j) $ srcPos y
    t -> errExpect (TEnum m i) t $ srcPos y
  TEnum n j -> inferFromExpr y >>= \case
    TTypedBus s m i
      | i == j     -> return ()
      | otherwise -> errExpect
                      (TTypedBus STGeneric n j)
                      (TTypedBus STGeneric m i)
                      (srcPos y)
    t -> errExpect (TTypedBus STGeneric n j) t $ srcPos y
  TPoly p -> inferFromExpr y >>= \case
    TNumber         -> updatePolyType p TNumber
    TEnum n j       -> updatePolyType p (TTypedBus STGeneric n j)
    TTypedBus _ n j -> updatePolyType p (TEnum n j)
    TPoly p'        -> equalPolyType p p'
    t               -> errEquality t $ srcPos y
  t -> errEquality t $ srcPos x

-----------------------------------------------------------------------------

typeChEqB
  :: Expression -> Expression -> TC ()

typeChEqB x y = inferFromExpr x >>= \case
  TNumber -> typeChck2 x y TNumber
  TPoly p -> inferFromExpr y >>= \case
    TNumber -> updatePolyType p TNumber
    TPoly p'-> updatePolyType p TNumber >> updatePolyType p' TNumber
    t       -> errExpect TNumber t $ srcPos y
  t       -> errExpect TNumber t $ srcPos x

-----------------------------------------------------------------------------

typeChck2
  :: Expression -> Expression -> ExprType -> TC ()

typeChck2 x y t = do
  typeCheck x t
  typeCheck y t

-----------------------------------------------------------------------------

typeChSEx
  :: ExprType -> [Expression] -> TC ()

typeChSEx t =
  mapM_ (flip typeCheck t)

-----------------------------------------------------------------------------

typeChSRg
  :: Expression -> Expression -> Expression -> TC ()

typeChSRg x y z = do
  typeCheck x TNumber
  typeCheck y TNumber
  typeCheck z TNumber

-----------------------------------------------------------------------------

typeChckS
  :: Expression -> TC ()

typeChckS x = do
  inferFromExpr x >>= typeCheck x

-----------------------------------------------------------------------------

typeChElm
  :: Expression -> Expression -> TC ()

typeChElm x xs = do
  inferFromExpr x >>= typeCheck x
  inferFromExpr x >>= (typeCheck xs . TSet)

-----------------------------------------------------------------------------

typeChckR
  :: Expression -> Expression -> StateT ST (Either Error) ()

typeChckR e x = case expr e of
  Colon n m -> do
    typeCheck n TNumber
    typeCheck m TNumber
    typeCheck x TLtl
  _         -> do
    typeCheck e TNumber
    typeCheck x TLtl

-----------------------------------------------------------------------------

typeChckU
  :: Expression -> Expression -> StateT ST (Either Error) ()

typeChckU e x = case expr e of
  Colon n m -> do
    typeCheck n TNumber
    typeCheck m TNumber
    typeCheck x TPattern
  _         -> do
    typeCheck e TNumber
    typeCheck x TPattern

-----------------------------------------------------------------------------

typeChckP
  :: Expression -> StateT ST (Either Error) ()

typeChckP (expr -> Pattern x y) = do
  typeCheck x TLtl
  typeCheck y TPattern

-----------------------------------------------------------------------------

typeChckO
  :: [Expression] -> Expression -> ExprType -> StateT ST (Either Error) ()

typeChckO xs y t = do
  mapM_ typeChckI xs
  typeCheck y t

-----------------------------------------------------------------------------

typeChckI
  :: Expression -> StateT ST (Either Error) ()

typeChckI = expr >>> \case
  BlnElem x xs                 -> typeChElm x xs
  BlnLE (expr -> BlnLE x y) z   -> typeChSRg x y z
  BlnLE (expr -> BlnLEQ x y) z  -> typeChSRg x y z
  BlnLEQ (expr -> BlnLE x y) z  -> typeChSRg x y z
  BlnLEQ (expr -> BlnLEQ x y) z -> typeChSRg x y z
  _                            -> assert False undefined

-----------------------------------------------------------------------------

typeErrES
  :: Expression -> Expression -> StateT ST (Either Error) ()

typeErrES e x = do
  t <- inferFromExpr x
  typeCheck x t
  errExpect TEmptySet (TSet t) $ srcPos e

-----------------------------------------------------------------------------

typeChSBn
  :: ExprType -> Expression -> Expression -> StateT ST (Either Error) ()

typeChSBn t x y = do
  typeCheck x $ TSet t
  typeCheck y $ TSet t

-----------------------------------------------------------------------------

inferFromExpr
  :: Expression -> StateT ST (Either Error) ExprType

inferFromExpr e = case expr e of
  BaseCon {}        -> return TNumber
  NumSMin {}        -> return TNumber
  NumSMax {}        -> return TNumber
  NumSSize {}       -> return TNumber
  NumSizeOf {}      -> return TNumber
  NumPlus {}        -> return TNumber
  NumMinus {}       -> return TNumber
  NumMul {}         -> return TNumber
  NumDiv {}         -> return TNumber
  NumMod {}         -> return TNumber
  NumRPlus {}       -> return TNumber
  NumRMul {}        -> return TNumber
  BaseWild          -> return TPattern
  BaseOtherwise     -> return TBoolean
  Pattern {}        -> return TBoolean
  BaseTrue          -> inferFromBoolExpr e
  BaseFalse         -> inferFromBoolExpr e
  BlnEQ {}          -> inferFromBoolExpr e
  BlnNEQ {}         -> inferFromBoolExpr e
  BlnGE {}          -> inferFromBoolExpr e
  BlnGEQ {}         -> inferFromBoolExpr e
  BlnLE {}          -> inferFromBoolExpr e
  BlnLEQ {}         -> inferFromBoolExpr e
  BlnNot {}         -> inferFromBoolExpr e
  BlnOr {}          -> inferFromBoolExpr e
  BlnROr {}         -> inferFromBoolExpr e
  BlnAnd {}         -> inferFromBoolExpr e
  BlnRAnd {}        -> inferFromBoolExpr e
  BlnImpl {}        -> inferFromBoolExpr e
  BlnElem {}        -> inferFromBoolExpr e
  BlnEquiv {}       -> inferFromBoolExpr e
  BaseBus {}        -> return TLtl
  LtlNext {}        -> return TLtl
  LtlRNext {}       -> return TLtl
  LtlGlobally {}    -> return TLtl
  LtlRGlobally {}   -> return TLtl
  LtlFinally {}     -> return TLtl
  LtlRFinally {}    -> return TLtl
  LtlUntil {}       -> return TLtl
  LtlWeak {}        -> return TLtl
  LtlRelease {}     -> return TLtl
  SetRange {}       -> return $ TSet TNumber
  SetExplicit []    -> return TEmptySet
  SetExplicit (x:_) -> liftM TSet $ inferFromExpr x
  SetCup x y        -> inferSetOp x y
  SetCap x y        -> inferSetOp x y
  SetMinus x y      -> inferSetOp x y
  SetRCap _ x       -> inferFromExpr x
  SetRCup _ x       -> inferFromExpr x
  Colon _ x         -> inferFromExpr x
  BaseId i          -> liftM (imLookup i . tTypes) get
  BaseFml _ i       -> liftM (imLookup i . tTypes) get

  where
    inferSetOp x y =
      inferFromExpr x >>= \case
        TEmptySet -> inferFromExpr y
        t         -> return t

-----------------------------------------------------------------------------

inferFromBoolExpr
  :: Expression -> StateT ST (Either Error) ExprType

inferFromBoolExpr e = case expr e of
  SetExplicit xs -> inferFromBoolSet xs
  SetCup x y     -> inferFromBoolE2 x y
  SetCap x y     -> inferFromBoolE2 x y
  SetMinus x y   -> inferFromBoolE2 x y
  BaseTrue       -> return TBoolean
  BaseFalse      -> return TBoolean
  BlnEQ x y      -> inferFromEqExpr x y
  BlnNEQ x y     -> inferFromEqExpr x y
  BlnGE {}       -> return TBoolean
  BlnGEQ {}      -> return TBoolean
  BlnLE {}       -> return TBoolean
  BlnLEQ {}      -> return TBoolean
  BlnElem {}     -> return TBoolean
  BlnNot x       -> inferFromBoolExpr x
  BlnOr x y      -> inferFromBoolE2 x y
  BlnROr _ x     -> inferFromBoolExpr x
  BlnAnd x y     -> inferFromBoolE2 x y
  BlnRAnd _ x    -> inferFromBoolExpr x
  BlnImpl x y    -> inferFromBoolE2 x y
  BlnEquiv x y   -> inferFromBoolE2 x y
  BaseId i       -> inferFromBoolId i
  BaseFml _ i    -> inferFromBoolId i
  _              -> inferFromExpr e >>= \case
    TSignal _ -> return TLtl
    TLtl      -> return TLtl
    _         -> return TBoolean

  where
    inferFromEqExpr
      :: Expression -> Expression -> TC ExprType

    inferFromEqExpr x y = inferFromExpr x >>= \case
      TTypedBus {} -> return TLtl
      TEnum {}     -> return TLtl
      TPoly p      -> inferFromExpr y >>= \case
        TTypedBus {} -> return TLtl
        TEnum {}     -> return TLtl
        TPoly p'     -> return TLtl
        _            -> return TBoolean
      _            -> return TBoolean

    inferFromBoolId
      :: ID -> TC ExprType

    inferFromBoolId i = do
      liftM (imLookup i . tTypes) get >>= \case
        TSignal _ -> return TLtl
        TLtl      -> return TLtl
        _         -> return TBoolean

    inferFromBoolE2
      :: Expression -> Expression -> TC ExprType

    inferFromBoolE2 x y = do
      inferFromBoolExpr x >>= \case
        TBoolean         -> inferFromBoolExpr y >>= \case
          TSignal _ -> return TLtl
          TLtl      -> return TLtl
          _         -> return TBoolean
        TSet TBoolean    -> inferFromBoolExpr y >>= \case
          TSet (TSignal _) -> return $ TSet TLtl
          TSet TLtl        -> return $ TSet TLtl
          _                -> return $ TSet TBoolean
        TSignal {}       -> return TLtl
        TSet (TSignal _) -> return $ TSet TLtl
        TLtl             -> return TLtl
        TSet (TLtl)      -> return $ TSet TLtl
        TSet _           -> return $ TSet TBoolean
        _                -> return TBoolean

    inferFromBoolSet
      :: [Expression] -> TC ExprType

    inferFromBoolSet =
      liftM TSet . foldM inferFromBoolElement TBoolean

    inferFromBoolElement
      :: ExprType -> Expression -> TC ExprType

    inferFromBoolElement = \case
      TBoolean -> \e -> do
        inferFromBoolExpr e >>= \case
          TSignal _ -> return TLtl
          TLtl      -> return TLtl
          _         -> return TBoolean
      TSignal _ -> const $ return TLtl
      TLtl      -> const $ return TLtl
      _         -> const $ return TBoolean

-----------------------------------------------------------------------------


{-inferLtl
  :: Specification -> TypeChecker [Int]

inferLtl s xs = do
  -- get bus parameters
  mapM_ inferBusParameter $ inputs s
  mapM_ inferBusParameter $ outputs s
  -- derive generic types
  let ys = map (\i -> (i,imLookup i $ bindings s)) xs
  mapM_ updateType ys
  -- process final LTL formulas
  mapM_ (inferFromUsage TLtl) $ initially s
  mapM_ (inferFromUsage TLtl) $ preset s
  mapM_ (inferFromUsage TLtl) $ requirements s
  mapM_ (inferFromUsage TLtl) $ assumptions s
  mapM_ (inferFromUsage TLtl) $ invariants s
  mapM_ (inferFromUsage TLtl) $ guarantees s
  checkTypes (arguments s) (bindings s) ys xs

  where
    -- the bus type must be number
    inferBusParameter
      :: TypeChecker (SignalDecType Int)

    inferBusParameter x = case x of
      SDBus _ e -> inferFromUsage TNumber e
      _         -> return ()

-----------------------------------------------------------------------------

checkTypes
  :: ArgumentTable -> ExpressionTable -> [(Int,Expression)] -> TypeChecker [Int]

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
  :: TypeChecker (Int,Expression)

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
  :: ArgumentTable -> TypeChecker (Int, Expression)

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

-- | Infers the type of an expression from its context.

inferFromUsage
  :: ExprType -> TypeChecker (Expression)

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
  :: ExprType -> TypeChecker (Expression)

inferFromUsageFormula t e = case expr e of
  Colon x y -> do
    inferFromUsage TBoolean x
    inferFromUsage t y
  _         ->
    inferFromUsage t e

-----------------------------------------------------------------------------

inferFromUsageNum
  :: TypeChecker (Expression)

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
  :: TypeChecker (Expression)

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
  :: TypeChecker (Expression)

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
  :: Expression -> TypeChecker (Expression)

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
  :: TypeChecker (Expression)

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
  :: TypeChecker (Expression)

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
  :: ExprType -> TypeChecker (Expression)

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
  :: TypeChecker (Expression)

inferFromUsageSignal e = case expr e of
  BaseId i     -> inferFromUsageId (TSignal STGeneric) (srcPos e) i
  BaseFml as i -> inferFromUsageFml as (TSignal STGeneric) (srcPos e) i
  _            -> do
    t <- inferFromExpr e
    errExpect (TSignal STGeneric) t $ srcPos e

-----------------------------------------------------------------------------

inferFromUsageBus
  :: TypeChecker (Expression)

inferFromUsageBus e = case expr e of
  BaseId i     -> inferFromUsageId (TBus STGeneric) (srcPos e) i
  BaseFml as i -> inferFromUsageFml as (TBus STGeneric) (srcPos e) i
  _            -> do
    t <- inferFromExpr e
    errExpect (TBus STGeneric) t $ srcPos e

-----------------------------------------------------------------------------

inferFromUsageTypedBus
  :: String -> Int -> TypeChecker (Expression)

inferFromUsageTypedBus s y e = case expr e of
  BaseId i     -> inferFromUsageId (TTypedBus STGeneric s y) (srcPos e) i
  BaseFml as i -> inferFromUsageFml as (TTypedBus STGeneric s y) (srcPos e) i
  _            -> do
    t <- inferFromExpr e
    errExpect (TTypedBus STGeneric s y) t $ srcPos e

-----------------------------------------------------------------------------

inferFromUsageEnum
  :: String -> Int -> TypeChecker (Expression)

inferFromUsageEnum s y e = case expr e of
  BaseId i     -> inferFromUsageId (TEnum s y) (srcPos e) i
  BaseFml as i -> inferFromUsageFml as (TEnum s y) (srcPos e) i
  _            -> do
    t <- inferFromExpr e
    errExpect (TEnum s y) t $ srcPos e

-----------------------------------------------------------------------------

inferFromUsageRange
  :: TypeChecker (Expression)

inferFromUsageRange e = case expr e of
  Colon x y -> mapM_ (inferFromUsage TNumber) [x,y]
  _         -> do
    t <- inferFromExpr e
    errRange t $ srcPos e

---

inferFromUsageId
  :: ExprType -> ExprPos -> TypeChecker Int

inferFromUsageId t pos i = do
  tt <- get
  let t' = imLookup i $ tTypes tt
  updExprType id t t'

  where
    updExprType c t1 t2 = case (t1,t2) of
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
        updExprType (c . TSet) a b
      _                  ->
        when (t1 /= t2 && ((t1 /= TLtl && noSignal t1) || noSignal t2)) $
          errExpect t1 t2 pos

    noSignal x = case x of
      TSignal _ -> False
      _         -> True

-----------------------------------------------------------------------------

inferFromUsageFml
  :: [Expression] -> ExprType -> ExprPos -> TypeChecker Int

inferFromUsageFml as t pos i = do
  inferFromUsageId t pos i
  st <- get
  let xs = zip as $ map (\x -> imLookup x $ tTypes st) $ imLookup i $ targs st
  mapM_ (\(x,y) -> inferFromUsage y x) xs

-----------------------------------------------------------------------------
-}
