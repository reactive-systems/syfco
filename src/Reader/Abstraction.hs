-----------------------------------------------------------------------------
-- |
-- Module      :  Reader.Abstraction
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- Abstracts from identifier names to integer IDs.
--
-----------------------------------------------------------------------------

module Reader.Abstraction
  ( abstract
  ) where

-----------------------------------------------------------------------------

import Data.Types
  ( SignalDecType(..)
  )

import Data.Enum
  ( EnumDefinition(..)
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
  ( NameTable
  , PositionTable
  , ArgumentTable
  , Specification(..)
  )

import Reader.Error
  ( Error
  , errUnknown
  , errConflict
  , errPattern
  )

import Data.Maybe
  ( mapMaybe
  )

import Control.Monad.State
  ( StateT(..)
  , evalStateT
  , get
  , put
  , void
  )

import qualified Reader.Parser.Data as PD
  ( Specification(..)
  )

import qualified Data.IntMap.Strict as IM
  ( empty
  , insert
  , lookup
  )

import qualified Data.StringMap as SM
  ( StringMap
  , empty
  , insert
  , lookup
  )

-----------------------------------------------------------------------------

type Abstractor a b = a -> StateT ST (Either Error) b

-----------------------------------------------------------------------------

data ST = ST
  { count :: Int
  , tIndex :: SM.StringMap
  , tName :: NameTable
  , tPos :: PositionTable
  , tArgs :: ArgumentTable
  }

-----------------------------------------------------------------------------

-- | Abstracts from identifiers represeted by strings to identifiers
-- represented by integers. Additionally, a mapping from the integer
-- representation back to the string representation as well as mapping to
-- possible arguments and the position in the source file is created.

abstract
  :: PD.Specification -> Either Error Specification

abstract spec =
  evalStateT (abstractSpec spec)
    ST { count = 1
       , tIndex = SM.empty
       , tName = IM.empty
       , tPos = IM.empty
       , tArgs = IM.empty
       }

-----------------------------------------------------------------------------

abstractSpec
  :: Abstractor PD.Specification Specification

abstractSpec s = do
  mapM_ (\x -> add (bIdent x,bPos x)) $ PD.parameters s
  mapM_ (mapM_ (\(y,z,_) -> add (y,z)) . eValues) $ PD.enumerations s
  mapM_ (\x -> add (bIdent x,bPos x)) $ PD.definitions s

  let
    (ig,ib,ie) = foldl classify ([],[],[]) $ PD.inputs s
    (og,ob,oe) = foldl classify ([],[],[]) $ PD.outputs s

  ig' <- mapM add ig
  og' <- mapM add og

  ps <- mapM abstractBind $ PD.parameters s
  vs <- mapM abstractBind $ PD.definitions s

  a <- get
  ms <- mapM abstractEnum $ PD.enumerations s
  ie' <- mapM abstractSignalType ie
  oe' <- mapM abstractSignalType oe
  a' <- get
  put a' {
    tIndex = tIndex a
    }

  ib' <- mapM abstractBus ib
  ob' <- mapM abstractBus ob
  ie'' <- mapM abstractTypedBus ie'
  oe'' <- mapM abstractTypedBus oe'

  let
    is =
      map SDSingle ig' ++
      map (uncurry SDBus) ib' ++
      map (uncurry SDEnum) ie''
    os =
      map SDSingle og' ++
      map (uncurry SDBus) ob' ++
      map (uncurry SDEnum) oe''

  es <- mapM abstractExpr $ PD.initially s
  ss <- mapM abstractExpr $ PD.preset s
  rs <- mapM abstractExpr $ PD.requirements s
  as <- mapM abstractExpr $ PD.assumptions s
  bs <- mapM abstractExpr $ PD.invariants s
  gs <- mapM abstractExpr $ PD.guarantees s

  st <- get

  return $ Specification
    (PD.title s)
    (PD.description s)
    (PD.semantics s)
    (PD.target s)
    (PD.tags s)
    ms ps vs is os es ss rs as bs gs
    IM.empty
    (tName st)
    (tPos st)
    (tArgs st)
    IM.empty
    IM.empty

  where
    classify (a,b,c) x = case x of
      SDSingle y -> (y:a,b,c)
      SDBus y z  -> (a,(y,z):b,c)
      SDEnum y z -> (a,b,(y,z):c)

    abstractTypedBus (n,m) = do
      a <- add n
      return (a,m)

-----------------------------------------------------------------------------

abstractSignalType
  :: Abstractor ((String,ExprPos),(String,ExprPos))
               ((String,ExprPos),(Int,ExprPos))

abstractSignalType (n,(t,p)) = do
  a <- get
  i <- case SM.lookup t $ tIndex a of
    Just j  -> return j
    Nothing -> errUnknown t p
  return (n,(i,p))

-----------------------------------------------------------------------------

abstractBus
  :: Abstractor ((String,ExprPos),Expr String)
               ((Int,ExprPos),Expr Int)

abstractBus ((s,p),e) = do
  (i,p') <- add (s,p)
  e' <- abstractExpr e
  return ((i,p'),e')

-----------------------------------------------------------------------------

abstractEnum
  :: Abstractor (EnumDefinition String) (EnumDefinition Int)

abstractEnum b = do
  (n,p) <- add (eName b, ePos b)
  vs <- mapM abstractEnumV $ eValues b

  return EnumDefinition
    { eName = n
    , eSize = eSize b
    , eValues = vs
    , ePos = p
    , eMissing = eMissing b
    , eDouble = Nothing
    }

  where
    abstractEnumV (n,p,f) = do
      a <- get
      case SM.lookup n $ tIndex a of
        Just j  -> return (j,p,f)
        Nothing -> errUnknown n p

-----------------------------------------------------------------------------

abstractBind
  :: Abstractor (BindExpr String) (BindExpr Int)

abstractBind b = do
  a <- get
  as <- mapM add $ bArgs b
  es <- mapM abstractExpr $ bVal b
  a' <- get
  i <- case SM.lookup (bIdent b) $ tIndex a of
    Just j  -> return j
    Nothing -> errUnknown (bIdent b) (bPos b)

  put $ a' {
    tIndex = tIndex a,
    tArgs = IM.insert i (map fst as) $ tArgs a'
    }

  return BindExpr
    { bIdent = i
    , bArgs = as
    , bPos = bPos b
    , bVal = es
    }

-----------------------------------------------------------------------------

add
  :: Abstractor (String,ExprPos) (Int,ExprPos)

add (i,pos) = do
  a <- get
  case SM.lookup i $ tIndex a of
    Nothing -> do
      put ST
        { count = count a + 1
        , tIndex = SM.insert i (count a) $ tIndex a
        , tName = IM.insert (count a) i $ tName a
        , tPos = IM.insert (count a) pos $ tPos a
        , tArgs = IM.insert (count a) [] $ tArgs a
        }
      return (count a,pos)
    Just j ->
      let Just p = IM.lookup j $ tPos a
      in errConflict i p pos

-----------------------------------------------------------------------------

check
  :: Abstractor (String,ExprPos) (Int,ExprPos)

check (i,pos) = do
  a <- get
  case SM.lookup i $ tIndex a of
    Nothing -> errUnknown i pos
    Just j  -> return (j,pos)

-----------------------------------------------------------------------------

abstractExpr
  :: Abstractor (Expr String) (Expr Int)

abstractExpr e = case expr e of
  BaseOtherwise    -> return $ Expr BaseOtherwise $ srcPos e
  BaseWild         -> return $ Expr BaseWild $ srcPos e
  BaseTrue         -> return $ Expr BaseTrue $ srcPos e
  BaseFalse        -> return $ Expr BaseFalse $ srcPos e
  BaseCon x        -> return $ Expr (BaseCon x) $ srcPos e
  BlnNot x         -> lift' BlnNot x
  NumSMin x        -> lift' NumSMin x
  NumSMax x        -> lift' NumSMax x
  NumSSize x       -> lift' NumSSize x
  NumSizeOf x      -> lift' NumSizeOf x
  LtlNext x        -> lift' LtlNext x
  LtlGlobally x    -> lift' LtlGlobally x
  LtlFinally x     -> lift' LtlFinally x
  NumPlus x y      -> lift2' NumPlus x y
  NumMinus x y     -> lift2' NumMinus x y
  NumMul x y       -> lift2' NumMul x y
  NumDiv x y       -> lift2' NumDiv x y
  NumMod x y       -> lift2' NumMod x y
  SetCup x y       -> lift2' SetCup x y
  SetCap x y       -> lift2' SetCap x y
  SetMinus x y     -> lift2' SetMinus x y
  BlnEQ x y        -> lift2' BlnEQ x y
  BlnNEQ x y       -> lift2' BlnNEQ x y
  BlnGE x y        -> lift2' BlnGE x y
  BlnGEQ x y       -> lift2' BlnGEQ x y
  BlnLE x y        -> lift2' BlnLE x y
  BlnLEQ x y       -> lift2' BlnLEQ x y
  BlnElem x y      -> lift2' BlnElem x y
  BlnOr x y        -> lift2' BlnOr x y
  BlnAnd x y       -> lift2' BlnAnd x y
  BlnImpl x y      -> lift2' BlnImpl x y
  BlnEquiv x y     -> lift2' BlnEquiv x y
  LtlRNext x y     -> lift2' LtlRNext x y
  LtlRGlobally x y -> lift2' LtlRGlobally x y
  LtlRFinally x y  -> lift2' LtlRFinally x y
  LtlUntil x y     -> lift2' LtlUntil x y
  LtlWeak x y      -> lift2' LtlWeak x y
  LtlRelease x y   -> lift2' LtlRelease x y
  NumRPlus xs x    -> cond NumRPlus xs x
  NumRMul xs x     -> cond NumRMul xs x
  SetRCup xs x     -> cond SetRCup xs x
  SetRCap xs x     -> cond SetRCap xs x
  BlnROr xs x      -> cond BlnROr xs x
  BlnRAnd xs x     -> cond BlnRAnd xs x
  BaseId x         -> do
    (x',p) <- check (x,srcPos e)
    return $ Expr (BaseId x') p
  BaseBus x y      -> do
    (y',p) <- check (y,srcPos e)
    x' <- abstractExpr x
    return $ Expr (BaseBus x' y') p
  BaseFml xs x     -> do
    (x',p) <- check (x,srcPos e)
    xs' <- mapM abstractExpr xs
    return $ Expr (BaseFml xs' x') p
  SetExplicit xs   -> do
    xs' <- mapM abstractExpr xs
    return $ Expr (SetExplicit xs') $ srcPos e
  SetRange x y z   -> do
    x' <- abstractExpr x
    y' <- abstractExpr y
    z' <- abstractExpr z
    return $ Expr (SetRange x' y' z') $ srcPos e
  Colon v z        -> case expr v of
    Pattern x y -> do
      a <- get
      x' <- abstractExpr x
      getPatternIds y
      y' <- abstractExpr y
      z' <- abstractExpr z
      a' <- get
      put $ a' { tIndex = tIndex a }
      return $ Expr (Colon (Expr (Pattern x' y') (srcPos v)) z') (srcPos e)
    _ -> lift2' Colon v z
  Pattern x y      -> lift2' Pattern x y

  where
    lift' c x = do
      x' <- abstractExpr x
      return $ Expr (c x') (srcPos e)

    lift2' c x y = do
      x' <- abstractExpr x
      y' <- abstractExpr y
      return $ Expr (c x' y') (srcPos e)

    cond c xs x = do
      a <- get
      mapM_ add $ mapMaybe getId xs
      xs' <- mapM abstractExpr xs
      x' <- abstractExpr x
      a' <- get
      put $ a' { tIndex = tIndex a }
      return $ Expr (c xs' x') (srcPos e)

    getId x = case expr x of
      BlnElem y _ -> isid y
      BlnLE n _   -> range n
      BlnLEQ n _  -> range n
      _          -> Nothing

    range n = case expr n of
      BlnLE _ m  -> isid m
      BlnLEQ _ m -> isid m
      _          -> Nothing

    isid m = case expr m of
      BaseId i -> Just (i,srcPos m)
      _        -> Nothing

    getPatternIds z = case expr z of
      BaseWild         -> return ()
      BaseTrue         -> return ()
      BaseFalse        -> return ()
      BaseOtherwise    -> return ()
      BaseId i         -> void $ add (i,srcPos z)
      BlnNot x         -> getPatternIds x
      BlnOr x y        -> mapM_ getPatternIds [x,y]
      BlnAnd x y       -> mapM_ getPatternIds [x,y]
      BlnImpl x y      -> mapM_ getPatternIds [x,y]
      BlnEquiv x y     -> mapM_ getPatternIds [x,y]
      LtlNext x        -> getPatternIds x
      LtlRNext _ x     -> getPatternIds x
      LtlGlobally x    -> getPatternIds x
      LtlRGlobally _ x -> getPatternIds x
      LtlFinally x     -> getPatternIds x
      LtlRFinally _ x  -> getPatternIds x
      LtlUntil x y     -> mapM_ getPatternIds [x,y]
      LtlWeak x y      -> mapM_ getPatternIds [x,y]
      LtlRelease x y   -> mapM_ getPatternIds [x,y]
      _                -> errPattern $ srcPos z

-----------------------------------------------------------------------------
