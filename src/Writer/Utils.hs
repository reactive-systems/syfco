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
    , partition  
    ) where

-----------------------------------------------------------------------------

import Data.LTL
    ( Atomic(..)
    , Formula(..)
    )
    
import Writer.Error
    ( Error
    )  

import Writer.Data
    ( WriteMode(..)
    , OperatorNames(..)  
    )  

-----------------------------------------------------------------------------

-- | Gernealized pretty printer that prints an expression using standard
-- semantics and the operations passed via @OperatorNames@.

pretty
  :: WriteMode -> OperatorNames -> Formula -> String

pretty mode ops = pr

  where
    parens x = "(" ++ x ++ ")"

    prUO p f = case f of
      And _       -> parens $ p f
      Or _        -> parens $ p f
      Implies _ _ -> parens $ p f
      Equiv _ _   -> parens $ p f
      Until _ _   -> parens $ p f
      Release _ _ -> parens $ p f
      _           -> p f

    prAnd p f = case f of
      Or _        -> parens $ p f
      Implies _ _ -> parens $ p f
      Equiv _ _   -> parens $ p f
      Until _ _   -> parens $ p f
      Release _ _ -> parens $ p f
      _           -> p f      

    prOr p f = case f of
      Implies _ _ -> parens $ p f
      Equiv _ _   -> parens $ p f
      Until _ _   -> parens $ p f
      Release _ _ -> parens $ p f
      _           -> p f      

    pr' = parens . pr
    
    prUO' = case mode of
      Pretty -> prUO pr
      _      -> pr'      

    prAnd' = case mode of
      Pretty -> prAnd pr      
      _      -> pr'      

    prOr' = case mode of
      Pretty -> prOr pr      
      _      -> pr'      

    pr f = case f of
      TTrue                   -> ptrue
      FFalse                  -> pfalse
      Not (Atomic (Input x))  -> pnot ++ prUO' (Atomic (Input x))
      Not (Atomic (Output x)) -> pnot ++ prUO' (Atomic (Output x))      
      Atomic (Input x)        -> x
      Atomic (Output x)       -> x      
      Not x                   -> pnot ++ prUO' x 
      And []                  -> pr TTrue
      And [x]                 -> pr x
      And (x:xr)              -> prAnd' x ++
                                concatMap (((" " ++ pand ++ " ") ++)
                                           . prAnd') xr 
      Or []                   -> pr FFalse
      Or [x]                  -> pr x  
      Or (x:xr)               -> prOr' x ++
                                concatMap (((" " ++ por ++ " ") ++)
                                           . prOr') xr
      Implies x y             -> prOr' x ++ " " ++ pimplies ++ " " ++ prOr' y
      Equiv x y               -> prOr' x ++ " " ++ pequiv ++ " " ++ prOr' y
      Next x                  -> pnext ++ " " ++ prUO' x    
      Globally x              -> pglobally ++ " " ++ prUO' x
      Finally x               -> pfinally ++ " " ++ prUO' x
      Until x y               -> prOr' x ++ " " ++ puntil ++ " " ++ prOr' y
      Release x y             -> prOr' x ++ " " ++ prelease ++ " " ++ prOr' y
      Weak x y                -> prOr' x ++ " " ++ pweak ++ " " ++ prOr' y

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
      ([x],_)   -> And ((Globally x) : gs)
      (_,[])    -> Globally $ And is
      (_,[x])   -> And [Globally $ And is, x]
      (_,_)     -> And ((Globally $ And is) : gs)
  in case as of
    []  -> return $ fml
    [x] -> return $ Implies x fml
    _   -> return $ Implies (And as) fml

-----------------------------------------------------------------------------

-- | Creates the contents of a standard partioning file from the lists
-- of input and output signals.

partition
  :: [String] -> [String] -> String

partition is os =
  ".inputs" ++ (concatMap (' ' :) is) ++ "\n" ++
  ".outputs" ++ (concatMap (' ' :) os) ++ "\n"

-----------------------------------------------------------------------------
