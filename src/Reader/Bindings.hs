-----------------------------------------------------------------------------
-- |
-- Module      :  Reader.Bindings
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
-- 
-- Extracts the static expression bindings from the specification.
-- 
-----------------------------------------------------------------------------

module Reader.Bindings
    ( specBindings
    ) where

-----------------------------------------------------------------------------

import Utils
    ( strictSort
    , imLookup
    )
    
import Data.Binding
    ( Binding
    , BindExpr(..)  
    )
    
import Data.Expression
    ( Expr(..)
    , Expr'(..)
    , SrcPos(..)
    , ExprPos(..)
    , subExpressions
    )  

import Reader.Data
    ( Specification(..)
    , ExpressionTable
    , NameTable
    , PositionTable
    , ArgumentTable 
    )
    
import Reader.Error
    ( Error
    , errArgArity
    , errConditional
    , errCircularDep  
    ) 

import Data.Graph
    ( buildG      
    , scc 
    )
    
import Data.Tree
    ( flatten
    )
    
import Data.Maybe
    ( fromJust
    )
    
import Control.Monad.State
    ( StateT(..)
    , execStateT
    , put
    , get
    , when  
    )  

import qualified Data.IntMap.Strict as IM

-----------------------------------------------------------------------------

type ArityTable = IM.IntMap Int

-----------------------------------------------------------------------------

type BindingsBuilder a = a -> StateT ST (Either Error) ()

-----------------------------------------------------------------------------

data ST = ST
  { tBinding :: ExpressionTable
  , tName :: NameTable
  , tPos :: PositionTable
  , tArgs :: ArgumentTable
  , tAry :: ArityTable  
  }

-----------------------------------------------------------------------------

-- | Extracts the static expression bindings from the specification and
-- stores them in a corresponding mapping. Furthermore, depencency
-- constraints are already checked.

specBindings
  :: Specification -> Either Error Specification

specBindings spec = do
  let
    p = SrcPos (-1) (-1)
    pos = ExprPos p p
    empty = Expr (SetExplicit []) pos
    a =
      ST { tBinding = IM.map (const empty) $ names spec
         , tName = names spec
         , tPos = positions spec
         , tArgs = arguments spec
         , tAry = IM.map length $ arguments spec
         }
      
  a' <- execStateT (specificationBindings spec) a
  
  checkCircularDeps spec
    { bindings = tBinding a'
    , dependencies = IM.map deps $ tBinding a'
    }

-----------------------------------------------------------------------------

specificationBindings
  :: BindingsBuilder Specification

specificationBindings s = do
  mapM_ binding $ parameters s
  mapM_ binding $ definitions s
  mapM_ binding $ inputs s
  mapM_ binding $ outputs s
  mapM_ exprBindings $ initially s
  mapM_ exprBindings $ preset s
  mapM_ exprBindings $ requirements s  
  mapM_ exprBindings $ invariants s
  mapM_ exprBindings $ assumptions s
  mapM_ exprBindings $ guarantees s   

-----------------------------------------------------------------------------

binding
  :: BindingsBuilder Binding

binding b = do
  mapM_ (\a -> addBinding (bIdent b,a)) $ bVal b
  mapM_ exprBindings $ bVal b

-----------------------------------------------------------------------------
        
exprBindings
  :: BindingsBuilder (Expr Int)

exprBindings e = case expr e of
  NumRPlus xs x -> mapM_ conditional xs >> exprBindings x
  NumRMul xs x  -> mapM_ conditional xs >> exprBindings x
  SetRCup xs x  -> mapM_ conditional xs >> exprBindings x
  SetRCap xs x  -> mapM_ conditional xs >> exprBindings x
  BlnRAnd xs x  -> mapM_ conditional xs >> exprBindings x
  BlnROr xs x   -> mapM_ conditional xs >> exprBindings x
  BaseId x      -> checkArity x 0
  BaseFml xs x  -> checkArity x (length xs)      
  BaseBus x y   -> checkArity y 0 >> exprBindings x
  _             -> mapM_ exprBindings $ subExpressions e

  where
    checkArity x j = do
      a <- get
      let n = imLookup x $ tAry a
      when (n /= j) $
        let m = imLookup x $ tName a
            p = imLookup x $ tPos a
        in errArgArity m n p $ srcPos e
    
    conditional x = case expr x of
      BlnElem l s -> case expr l of
        BaseId i -> do
          a <- get
          put a {
            tBinding = IM.insert i s $ tBinding a
            }
        _        -> errConditional $ srcPos e
      BlnLE s r -> case expr s of
        BlnLE l i  -> range i (op NumPlus l) (op NumMinus r) $ srcPos x
        BlnLEQ l i -> range i l (op NumMinus r) $ srcPos x
        _          -> errConditional $ srcPos e
      BlnLEQ s r -> case expr s of
        BlnLE l i  -> range i (op NumPlus l) r $ srcPos x
        BlnLEQ l i -> range i l r $ srcPos x
        _          -> errConditional $ srcPos e
      _           -> errConditional $ srcPos e

    op c x = Expr (c x (Expr (BaseCon 1) (srcPos x))) (srcPos x)

    range x l u p = case expr x of
      BaseId i -> do
        a <- get
        let s = Expr (SetRange l (op NumPlus l) u) p
        put a {
          tBinding = IM.insert i s $ tBinding a
          }
      _        -> errConditional $ srcPos e

-----------------------------------------------------------------------------

addBinding
  :: BindingsBuilder (Int,Expr Int)

addBinding (i,x) = do
  a <- get
  let y = imLookup i $ tBinding a
      z = Expr (SetExplicit [x]) $ srcPos x
      s = Expr (SetCup y z) $ srcPos y
  put a {
    tBinding = IM.insert i s $ tBinding a
    }

-----------------------------------------------------------------------------

deps
  :: Expr Int -> [Int]

deps = strictSort . deps' []
  where
    deps' a e = case expr e of
      BaseFml xs x  -> foldl conditional (x:a) xs 
      BaseId x      -> x:a
      BaseBus x y   -> deps' (y:a) x
      NumRPlus xs x -> foldl conditional (deps' a x) xs 
      NumRMul xs x  -> foldl conditional (deps' a x) xs 
      SetRCup xs x  -> foldl conditional (deps' a x) xs
      SetRCap xs x  -> foldl conditional (deps' a x) xs       
      BlnRAnd xs x  -> foldl conditional (deps' a x) xs
      BlnROr xs x   -> foldl conditional (deps' a x) xs
      Colon x y     -> case expr x of
        Pattern z _ -> deps' (deps' a z) y
        _           -> deps' (deps' a x) y
      _              -> foldl deps' a $ subExpressions e
      
    conditional k x = case expr x of
      BlnElem _ y -> deps' k y
      _           -> deps' k x

-----------------------------------------------------------------------------

checkCircularDeps
  :: Specification -> Either Error Specification

checkCircularDeps s = do
  let
    ys = concatMap (\(i,zs) -> map (\z -> (i,z)) zs) $
         filter isunary $ IM.toList $
         dependencies s
    minkey =
      if IM.null $ dependencies s then 0 else
        fst $ fst $ fromJust $ IM.minViewWithKey $ dependencies s
    maxkey =
      if IM.null $ dependencies s then 0 else
        fst $ fst $ fromJust $ IM.maxViewWithKey $ dependencies s
    c = map flatten $ scc $ buildG (minkey,maxkey) ys

  mapM_ check c
  mapM_ checkSingelton ys
  return s

  where
    isunary (x,_) = null $ imLookup x $ arguments s
    
    check xs = when (length xs > 1) $  
      let
        p = imLookup (head xs) $ positions s
        ys = map (\i -> (imLookup i $ names s, imLookup i $ positions s)) xs
      in
        errCircularDep ys p

    checkSingelton (i,j) = when (i == j) $
      errCircularDep [(imLookup i $ names s, imLookup i $ positions s)] $
      imLookup i $ positions s

-----------------------------------------------------------------------------

