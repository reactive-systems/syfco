-----------------------------------------------------------------------------
-- |
-- Module      :  Writer.Utils
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
-- 
-- Functions shared among the different writer modules.
-- 
-----------------------------------------------------------------------------

module Writer.Utils
    ( pretty
    , merge
    , checkLower  
    ) where

-----------------------------------------------------------------------------

import Data.Char
    ( toLower
    )

import Data.LTL
    ( Atomic(..)
    , Formula(..)
    )

import Data.Binding
    ( BindExpr(..)
    )

import Data.SymbolTable
    ( IdRec(..)
    )  

import Data.Specification
    ( Specification(..)
    )
    
import Writer.Error
    ( Error
    , errToLower
    )

import Writer.Data
    ( WriteMode(..)
    , OperatorNames(..)  
    )
    
import Data.Array.IArray
    ( (!)
    )  

-----------------------------------------------------------------------------

-- | Gernealized pretty printer that prints an expression using standard
-- semantics and the operations passed via @OperatorNames@.

pretty
  :: WriteMode -> OperatorNames -> Formula -> String

pretty mode ops = reverse . pr []

  where
    parens c a x = ')' : c ('(':a) x 

    prUO p a f = case f of
      And _       -> parens p a f
      Or _        -> parens p a f
      Implies _ _ -> parens p a f
      Equiv _ _   -> parens p a f
      Until _ _   -> parens p a f
      Release _ _ -> parens p a f
      _           -> p a f

    prAnd p a f = case f of
      Or _        -> parens p a f
      Implies _ _ -> parens p a f
      Equiv _ _   -> parens p a f
      Until _ _   -> parens p a f
      Release _ _ -> parens p a f
      _           -> p a f     

    prOr p a f = case f of
      Implies _ _ -> parens p a f
      Equiv _ _   -> parens p a f
      Until _ _   -> parens p a f
      Release _ _ -> parens p a f
      _           -> p a f      

    pr' = parens pr
    
    prUO' = case mode of
      Pretty -> prUO pr
      _      -> pr'

    prAnd' = case mode of
      Pretty -> prAnd pr      
      _      -> pr'      

    prOr' = case mode of
      Pretty -> prOr pr      
      _      -> pr'      

    pr a f = case f of
      TTrue                   -> revappend a ptrue
      FFalse                  -> revappend a pfalse
      Not (Atomic (Input x))  -> prUO' (revappend a pnot) (Atomic (Input x))
      Not (Atomic (Output x)) -> prUO' (revappend a pnot) (Atomic (Output x))      
      Atomic (Input x)        -> revappend a x
      Atomic (Output x)       -> revappend a x      
      Not x                   -> prUO' (revappend a pnot) x
      And []                  -> pr a TTrue
      And [x]                 -> pr a x
      And (x:xr)              -> foldl (\b y -> prAnd' (' ' : revappend (' ' : b) pand) y) (prAnd' a x) xr
      Or []                   -> pr a FFalse
      Or [x]                  -> pr a x  
      Or (x:xr)               -> foldl (\b y -> prOr' (' ' : revappend (' ' : b) por) y) (prOr' a x) xr
      Implies x y             -> prOr' (' ':(revappend (' ':(prOr' a x)) pimplies)) y 
      Equiv x y               -> prOr' (' ':(revappend (' ':(prOr' a x)) pequiv)) y 
      Next x                  -> prUO' (' ':(revappend a pnext)) x    
      Globally x              -> prUO' (' ':(revappend a pglobally)) x
      Finally x               -> prUO' (' ':(revappend a pfinally)) x
      Until x y               -> prOr' (' ':(revappend (' ':(prOr' a x)) puntil)) y
      Release x y             -> prOr' (' ':(revappend (' ':(prOr' a x)) prelease)) y 
      Weak x y                -> prOr' (' ':(revappend (' ':(prOr' a x)) pweak)) y 

    ptrue = opTrue ops
    pfalse = opFalse ops
    pnot = opNot ops
    pand = opAnd ops
    por = opOr ops
    pimplies = opImplies ops
    pequiv = opEquiv ops
    pnext = opNext ops
    pfinally = opFinally ops
    pglobally = opGlobally ops
    puntil = opUntil ops
    prelease = opRelease ops
    pweak = opWeak ops

    revappend a xs = case xs of
      []     -> a
      (x:xr) -> revappend (x:a) xr

-----------------------------------------------------------------------------

-- | Merges a list of assumption formulas, invariant formulas and guarantee
-- formulas to one single formula without introducing any overhead in case
-- one of the lists is empty or a singleton.

merge
  :: [Formula] -> [Formula] -> [Formula] -> Either Error Formula

merge as is gs =
  let
    fml = case (is,gs) of
      ([],[])   -> TTrue
      ([],[x])  -> x
      ([],_)    -> And gs
      ([x],[])  -> Globally x
      ([x],[y]) -> And [Globally x,y]
      ([x],_)   -> And (Globally x : gs)
      (_,[])    -> Globally $ And is
      (_,[x])   -> And [Globally $ And is, x]
      (_,_)     -> And ((Globally $ And is) : gs)
  in case as of
    []  -> return fml
    [x] -> return $ Implies x fml
    _   -> return $ Implies (And as) fml

-----------------------------------------------------------------------------

-- | Checks whether a conversion of the signal names to lower case would
-- introduce any clash.

checkLower
  :: String -> Specification -> Either Error ()

checkLower fmt s =
  let
    ids = map bIdent (inputs s) ++
          map bIdent (outputs s)
    names = map (idName . (symboltable s !)) ids
    lnames = map (map toLower) names
    znames = zip3 ids names lnames
  in
    checkDouble znames
    
  where
    checkDouble xs = case xs of
      [] ->  return ()
      [_] -> return ()
      ((i,a,b):(x,c,d):xr) ->
        if b == d 
        then errToLower fmt a c $ idPos $ symboltable s ! i
        else checkDouble ((x,c,d) : xr)

-----------------------------------------------------------------------------
