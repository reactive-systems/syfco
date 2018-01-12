-----------------------------------------------------------------------------
-- |
-- Module      :  Reader.InferType
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- Infers and checks types of all bound expressions.
--
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

import Control.Arrow
  ( (>>>)
  , (&&&)
  )

import Control.Monad
  ( (>=>)
  , foldM
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
  ( BindExpr(..)
  )

import Data.Expression
  ( Expr(..)
  , Expr'(..)
  , ExprPos
  )

import Reader.Data
  ( TypeTable
  , ArgumentTable
  , Specification(..)
  )

import Reader.Error
  ( Error
  , errExpect
  , errNoPFuns
  , errArgArity
  , errNoHigher
  , errEquality
  , errInfinite
  )

import Control.Monad.State
  ( StateT(..)
  , execStateT
  , get
  , put
  , when
  , unless
  )

import Control.Exception
  ( assert
  )

import qualified Data.IntMap.Strict as IM
  ( map
  )

import Data.IntMap.Strict
  ( (!)
  , insert
  , fromList
  , findMax
  , mapWithKey
  )

-----------------------------------------------------------------------------

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
  let st = ST { tCount = (+1) $ fst $ findMax names
              , tTypes = mapWithKey (flip $ const TPoly) names
              , targs = arguments
              , spec = s
              , curBinding = -1
              }

  tt <- execStateT inferTypeSpec st
  return $ s { types = tTypes tt }

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
      fromList $
        map (bIdent &&& Left) parameters ++
        map (bIdent &&& Right) definitions

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
  s@Specification{..} <- spec <$> get

  -- set the enumeration types
  mapM_ setEnumType enumerations
  -- set the signal types fo input and outputs
  mapM_ (setSignalType STInput) inputs
  mapM_ (setSignalType STOutput) outputs
  -- set the parameter types
  mapM_ (updType TNumber . bIdent) parameters

  -- fix and check the binding types
  mapM_ typeCheckBinding $ depOrderedBindings s
  -- type-check bus-parameters
  mapM_ typeCheckBusParameter inputs
  mapM_ typeCheckBusParameter outputs
  -- type-check LTL formulas
  mapM_ (`typeCheck` TLtl) initially
  mapM_ (`typeCheck` TLtl) preset
  mapM_ (`typeCheck` TLtl) requirements
  mapM_ (`typeCheck` TLtl) assumptions
  mapM_ (`typeCheck` TLtl) invariants
  mapM_ (`typeCheck` TLtl) guarantees

  where
    setSignalType
      :: SignalType -> SignalDecType ID -> TC ()

    setSignalType s = \case
      SDSingle (x,_)     -> updType (TSignal s) x
      SDBus (x,_) _      -> updType (TBus s) x
      SDEnum (x,_) (y,_) -> do
        Specification{..} <- spec <$> get
        updType (TTypedBus s (imLookup y names) y) x


    setEnumType
      :: EnumDefinition ID -> TC ()

    setEnumType e = do
      Specification{..} <- spec <$> get

      mapM_
        (updType (TEnum (imLookup (eName e) names) $ eName e)
          . (\(y,_,_) -> y))
        (eValues e)

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
  Specification{..} <- spec <$> get

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
  e <- ((! i) . bindings . spec) <$> get

  -- tyepcheck and infer types
  check e 10

  -- reset the recursion indicator
  setCur (-1)

  where
    setCur
      :: Int -> TC ()

    setCur c = do
      st <- get
      put st
        { curBinding = c
        }


    inferBinding
      :: Expression -> TC ExprType

    inferBinding e = inferFromExpr e >>= \case
      TSet TBoolean -> inferFromBoolExpr e
      t'            -> return t'


    check
      :: Expression -> Int -> TC ()

    check e n = do
      -- get the type of the expression
      TSet t <- inferBinding e
      -- update the type in the type table
      updType t i
      -- typecheck the expression
      typeCheck e $ TSet t

      if n > 0
      then check e (n-1)
      else do
        TSet t' <- inferBinding e

        when (t /= t') $ do
          n <- ((! i) . names . spec) <$> get
          p <- ((! i) . positions . spec) <$> get
          errInfinite n p

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
    Pattern x y       -> typeChckP x y
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
  _            -> do
    TSet t' <- inferFromExpr e
    errExpect t t' $ srcPos e

-----------------------------------------------------------------------------

typeCheckBus
  :: ExprType -> Expression -> ID -> ExprPos -> TC ()

typeCheckBus t n b p = do
  typeCheck n TNumber

  ((! b) . tTypes) <$> get >>= \case
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
  as <- ((! i) . arguments . spec) <$> get

  -- higher order functions are not supported
  unless (null as) $ do
    Specification{..} <- spec <$> get
    errNoHigher (names ! i) p

  tt <- tTypes <$> get
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
    vt TEmptySet           (TSet _)            = return ()
    vt (TSet s)            TEmptySet           = updType (TSet s) i
    vt (TSet s)            (TSet s')           = vt s s'
    vt TLtl                TBoolean            = return ()
    vt TLtl                (TSignal _)         = return ()
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
  pf <- ((! f) . positions . spec) <$> get
  -- get athe rguments of f
  as <- ((! f) . arguments . spec) <$> get
  -- get current type table
  tt <- tTypes <$> get
  -- get argument types of f
  let at = map (tt !) as

  -- check argument arity
  when (length as /= length xs) $
    errArgArity (show $ length xs) (length as) pf p

  -- typecheck the arguments
  mapM_ (uncurry typeCheck) $ zip xs at
  -- get the result type
  rt <- ((! f) . tTypes) <$> get
  -- reset bound types for dependant functions (polymorphism)
  resetDependencies tt f
  -- check the compatibility of the result type
  validTypes p f t rt

-----------------------------------------------------------------------------

resetDependencies
  :: TypeTable -> ID -> TC ()

resetDependencies tt x = do
  i <- curBinding <$> get
  as <- (S.fromList . (! x) . arguments . spec) <$> get
  ds <- (S.fromList . (! x) . dependencies . spec) <$> get

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
  put st { tTypes = insert i (tt ! i) tTypes }

-----------------------------------------------------------------------------

updType
  :: ExprType -> ID -> TC ()

updType t i = do
  st@ST{..} <- get
  put st { tTypes = insert i t tTypes }

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
  TTypedBus _ m i -> inferFromExpr y >>= \case
    TEnum n j
      | i == j     -> return ()
      | otherwise -> errExpect (TEnum m i) (TEnum n j) $ srcPos y
    t -> errExpect (TEnum m i) t $ srcPos y
  TEnum n j -> inferFromExpr y >>= \case
    TTypedBus _ m i
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
  mapM_ (`typeCheck` t)

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

typeChckS x =
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
  :: Expression -> Expression -> StateT ST (Either Error) ()

typeChckP x y = do
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
  SetExplicit (x:_) -> TSet <$> inferFromExpr x
  SetCup x y        -> inferSetOp x y
  SetCap x y        -> inferSetOp x y
  SetMinus x y      -> inferSetOp x y
  SetRCap _ x       -> inferFromExpr x
  SetRCup _ x       -> inferFromExpr x
  Colon _ x         -> inferFromExpr x
  BaseId i          -> (imLookup i . tTypes) <$> get
  BaseFml _ i       -> (imLookup i . tTypes) <$> get

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
      TPoly {}     -> inferFromExpr y >>= \case
        TTypedBus {} -> return TLtl
        TEnum {}     -> return TLtl
        TPoly {}     -> return TLtl
        _            -> return TBoolean
      _            -> return TBoolean

    inferFromBoolId
      :: ID -> TC ExprType

    inferFromBoolId i =
      (imLookup i . tTypes) <$> get >>= \case
        TSignal _ -> return TLtl
        TLtl      -> return TLtl
        _         -> return TBoolean

    inferFromBoolE2
      :: Expression -> Expression -> TC ExprType

    inferFromBoolE2 x y =
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
        TSet TLtl        -> return $ TSet TLtl
        TSet _           -> return $ TSet TBoolean
        _                -> return TBoolean

    inferFromBoolSet
      :: [Expression] -> TC ExprType

    inferFromBoolSet =
      fmap TSet . foldM inferFromBoolElement TBoolean

    inferFromBoolElement
      :: ExprType -> Expression -> TC ExprType

    inferFromBoolElement = \case
      TBoolean ->
        inferFromBoolExpr >=> \case
          TSignal _ -> return TLtl
          TLtl      -> return TLtl
          _         -> return TBoolean
      TSignal _ -> const $ return TLtl
      TLtl      -> const $ return TLtl
      _         -> const $ return TBoolean

-----------------------------------------------------------------------------
