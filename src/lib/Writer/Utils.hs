-----------------------------------------------------------------------------
-- |
-- Module      :  Writer.Utils
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- Functions shared among the different writer modules.
--
-----------------------------------------------------------------------------

{-# Language

    LambdaCase

  #-}

-----------------------------------------------------------------------------

module Writer.Utils
    ( printFormula
    , checkLower
    , merge
    , adjust
    ) where

-----------------------------------------------------------------------------

import Data.Maybe
    ( mapMaybe
    )

import Data.Char
    ( toLower
    )

import Config
    ( Configuration(..)
    )

import Data.Types
    ( SignalDecType(..)
    )

import Data.LTL
    ( Atomic(..)
    , Formula(..)
    , fNot
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
    , errUnsupportedOp
    )

import Writer.Data
    ( WriteMode(..)
    , QuoteMode(..)
    , OperatorConfig(..)
    , UnaryOperator(..)
    , BinaryOperator(..)
    , Assoc(..)
    , Unsupported(..)
    )

import Data.Array.IArray
    ( (!)
    )

-----------------------------------------------------------------------------

-- | Gernealized printer that supports pretty printing and printing
-- fully parenthesized formulas for arbitrary operator configurations,
-- passed via @OperatorNames@.

printFormula
  :: OperatorConfig -> WriteMode -> QuoteMode -> Formula -> Either Error String

printFormula opc writeMode quoteMode formula = do
  checkSupported formula
  return $ reverse $ case writeMode of
    Pretty -> pr [] formula
    Fully  -> parens pr [] formula

  where
    checkSupported
      :: Formula -> Either Error ()

    checkSupported = \case
      TTrue                          -> return ()
      FFalse                         -> return ()
      Atomic {}                      -> return ()
      Not x
        | unsupported opnot          -> errUnsupportedOp "Boolean operator: \"not\""
        | otherwise                  -> checkSupported x
      And []
        | unsupported opand          -> errUnsupportedOp "Boolean operator: \"and\""
        | otherwise                  -> return ()
      And (x:xr)
        | unsupported opand          -> errUnsupportedOp "Boolean operator: \"and\""
        | otherwise                  -> checkSupported x >> checkSupported (And xr)
      Or []
        | unsupported opor           -> errUnsupportedOp "Boolean operator: \"or\""
        | otherwise                  -> return ()
      Or (x:xr)
        | unsupported opor           -> errUnsupportedOp "Boolean operator: \"or\""
        | otherwise                  -> checkSupported x >> checkSupported (Or xr)
      Implies x y
        | unsupported opimplies      -> errUnsupportedOp "Boolean operator: \"implication\""
        | otherwise                  -> checkSupported x >> checkSupported y
      Equiv x y
        | unsupported opequiv        -> errUnsupportedOp "Boolean operator: \"equivalence\""
        | otherwise                  -> checkSupported x >> checkSupported y
      StrongNext x
        | unsupported opstrongnext   -> errUnsupportedOp "temporal operator: \"strong next\""
        | otherwise                  -> checkSupported x
      Next x
        | unsupported opnext         -> errUnsupportedOp "temporal operator: \"next\""
        | otherwise                  -> checkSupported x
      WeakNext x
        | unsupported opweaknext     -> errUnsupportedOp "temporal operator: \"weak next\""
        | otherwise                  -> checkSupported x
      Previous x
        | unsupported opprevious     -> errUnsupportedOp "past LTL operator: \"previous\""
        | otherwise                  -> checkSupported x
      Globally x
        | unsupported opglobally     -> errUnsupportedOp "temporal operator: \"globally\""
        | otherwise                  -> checkSupported x
      Finally x
        | unsupported opfinally      -> errUnsupportedOp "temporal operator: \"finally\""
        | otherwise                  -> checkSupported x
      Historically x
        | unsupported ophistorically -> errUnsupportedOp "past LTL operator: \"historically\""
        | otherwise                  -> checkSupported x
      Once x
        | unsupported oponce         -> errUnsupportedOp "past LTL operator: \"once\""
        | otherwise                  -> checkSupported x
      Until x y
        | unsupported opuntil        -> errUnsupportedOp "temporal operator: \"until\""
        | otherwise                  -> checkSupported x >> checkSupported y
      Release x y
        | unsupported oprelease      -> errUnsupportedOp "temporal operator: \"release\""
        | otherwise                  -> checkSupported x >> checkSupported y
      Weak x y
        | unsupported opweak         -> errUnsupportedOp "temporal operator: \"weak until\""
        | otherwise                  -> checkSupported x >> checkSupported y
      Since x y
        | unsupported opsince        -> errUnsupportedOp "past LTL operator: \"since\""
        | otherwise                  -> checkSupported x >> checkSupported y
      Triggered x y
        | unsupported optriggered    -> errUnsupportedOp "past LTL operator: \"triggered\""
        | otherwise                  -> checkSupported x >> checkSupported y

    quote x = case quoteMode of
        NoQuotes -> x
        DoubleQuotes -> "\"" ++ x ++ "\""

    parens c a x = ')' : c ('(':a) x

    pr' a y r x = case writeMode of
      Fully  -> parens pr a x
      Pretty -> case compare (precedence' y) (precedence' x) of
        LT -> parens pr a x
        GT -> pr a x
        EQ -> case (assoc' y, r) of
          (AssocRight, False) -> parens pr a x
          (AssocRight, True)  -> pr a x
          (_, False)          -> pr a x
          (_, True)           -> parens pr a x

    pr a f = case f of
      TTrue                   -> revappend a ttrue
      FFalse                  -> revappend a ffalse
      Atomic (Input x)        -> revappend a (quote x)
      Atomic (Output x)       -> revappend a (quote x)
      Not x                   -> pr' (unOp opnot a) f False x
      And []                  -> pr a TTrue
      And [x]                 -> pr a x
      And [x,y]               -> pr' (binOp opand $ pr' a f False x) f True y
      And (x:y:xr)            -> case assoc' f of
        AssocRight -> pr' (binOp opand $ pr' a f False x) f True $ And (y:xr)
        _          -> pr a $ And $ And [x,y] : xr
      Or []                   -> pr a FFalse
      Or [x]                  -> pr a x
      Or [x,y]                -> pr' (binOp opor $ pr' a f False x) f True y
      Or (x:y:xr)             -> case assoc' f of
        AssocRight -> pr' (binOp opor $ pr' a f False x) f True $ Or (y:xr)
        _          -> pr a $ Or $ Or [x,y] : xr
      Implies x y             -> pr' (binOp opimplies $ pr' a f False x) f True y
      Equiv x y               -> pr' (binOp opequiv $ pr' a f False x) f True y
      Next x                  -> pr' (unOp opnext a) f True x
      StrongNext x            -> pr' (unOp opstrongnext a) f True x
      WeakNext x              -> pr' (unOp opweaknext a) f True x
      Previous x              -> pr' (unOp opprevious a) f True x
      Globally x              -> pr' (unOp opglobally a) f True x
      Finally x               -> pr' (unOp opfinally a) f True x
      Historically x          -> pr' (unOp ophistorically a) f True x
      Once x                  -> pr' (unOp oponce a) f True x
      Until x y               -> pr' (binOp opuntil $ pr' a f False x) f True y
      Release x y             -> pr' (binOp oprelease $ pr' a f False x) f True y
      Weak x y                -> pr' (binOp opweak $ pr' a f False x) f True y
      Since x y               -> pr' (binOp opsince $ pr' a f False x) f True y
      Triggered x y           -> pr' (binOp optriggered $ pr' a f False x) f True y

    precedence' f = case f of
      TTrue           -> 0
      FFalse          -> 0
      Atomic {}       -> 0
      Not {}          -> dp + uopPrecedence opnot
      And {}          -> dp + bopPrecedence opand
      Or {}           -> dp + bopPrecedence opor
      Implies {}      -> dp + bopPrecedence opimplies
      Equiv {}        -> dp + bopPrecedence opequiv
      Next {}         -> dp + uopPrecedence opnext
      StrongNext {}   -> dp + uopPrecedence opstrongnext
      WeakNext {}     -> dp + uopPrecedence opweaknext
      Previous {}     -> dp + uopPrecedence opprevious
      Globally {}     -> dp + uopPrecedence opglobally
      Finally {}      -> dp + uopPrecedence opfinally
      Historically {} -> dp + uopPrecedence ophistorically
      Once {}         -> dp + uopPrecedence oponce
      Until {}        -> dp + bopPrecedence opuntil
      Release {}      -> dp + bopPrecedence oprelease
      Weak {}         -> dp + bopPrecedence opweak
      Since {}        -> dp + bopPrecedence opsince
      Triggered {}    -> dp + bopPrecedence optriggered

    assoc' f = case f of
      TTrue           -> AssocLeft
      FFalse          -> AssocLeft
      Atomic {}       -> AssocLeft
      Not {}          -> AssocLeft
      And {}          -> bopAssoc opand
      Or {}           -> bopAssoc opor
      Implies {}      -> bopAssoc opimplies
      Equiv {}        -> bopAssoc opequiv
      Next {}         -> AssocLeft
      StrongNext {}   -> AssocLeft
      WeakNext {}     -> AssocLeft
      Previous {}     -> AssocLeft
      Globally {}     -> AssocLeft
      Finally {}      -> AssocLeft
      Historically {} -> AssocLeft
      Once {}         -> AssocLeft
      Until {}        -> bopAssoc opuntil
      Release {}      -> bopAssoc oprelease
      Weak {}         -> bopAssoc opweak
      Since {}        -> bopAssoc opsince
      Triggered {}    -> bopAssoc optriggered

    binOp op a = ' ' : revappend (' ' : a) (bopName op)

    unOp op a = ' ' : revappend a (uopName op)

    uopPrecedence' x =
      if unsupported x
      then Nothing
      else Just $ uopPrecedence x

    bopPrecedence' x =
      if unsupported x
      then Nothing
      else Just $ bopPrecedence x

    dp =
      let
        xs = mapMaybe (\f -> f opc)
          [ uopPrecedence' . opNot
          , bopPrecedence' . opAnd
          , bopPrecedence' . opOr
          , bopPrecedence' . opImplies
          , bopPrecedence' . opEquiv
          , uopPrecedence' . opNext
          , uopPrecedence' . opStrongNext
          , uopPrecedence' . opWeakNext
          , uopPrecedence' . opFinally
          , uopPrecedence' . opGlobally
          , bopPrecedence' . opUntil
          , bopPrecedence' . opRelease
          , bopPrecedence' . opWeak
          ]
        m = foldl min 1 xs
      in if m > 0
         then 0
         else 1 - m

    ttrue = tTrue opc
    ffalse = fFalse opc
    opnot = opNot opc
    opand = opAnd opc
    opor = opOr opc
    opimplies = opImplies opc
    opequiv = opEquiv opc
    opnext = opNext opc
    opstrongnext = opStrongNext opc
    opweaknext = opWeakNext opc
    opprevious = opPrevious opc
    opfinally = opFinally opc
    opglobally = opGlobally opc
    ophistorically = opHistorically opc
    oponce = opOnce opc
    opuntil = opUntil opc
    oprelease = opRelease opc
    opweak = opWeak opc
    opsince = opSince opc
    optriggered = opTriggered opc

    revappend a xs = case xs of
      []     -> a
      (x:xr) -> revappend (x:a) xr

-----------------------------------------------------------------------------

-- | Merges a list of assumption formulas, invariant formulas and guarantee
-- formulas to one single formula without introducing any overhead in case
-- one of the lists is empty or a singleton.

merge
  :: [Formula] -> [Formula] -> [Formula] ->
    [Formula] -> [Formula] -> [Formula] -> Either Error Formula

merge es ss rs as is gs =
  let
    fmle = case (rs,as) of
      ([],[])   -> TTrue
      ([],[x])  -> x
      ([],_)    -> And as
      ([x],[])  -> Globally x
      ([x],[y]) -> And [Globally x,y]
      ([x],_)   -> And (Globally x : as)
      (_,[])    -> Globally $ And rs
      (_,[x])   -> And [Globally $ And rs, x]
      (_,_)     -> And ((Globally $ And rs) : as)

    fmls = case (is,gs) of
      ([],[])   -> TTrue
      ([],[x])  -> x
      ([],_)    -> And gs
      ([x],[])  -> Globally x
      ([x],[y]) -> And [Globally x,y]
      ([x],_)   -> And (Globally x : gs)
      (_,[])    -> Globally $ And is
      (_,[x])   -> And [Globally $ And is, x]
      (_,_)     -> And ((Globally $ And is) : gs)

    fmli = case (fmle, fmls) of
      (FFalse,_) -> TTrue
      (TTrue,x)  -> x
      (x,FFalse) -> fNot x
      (_,TTrue)  -> TTrue
      _          -> Implies fmle fmls

    fmlc = case (ss,fmli) of
      ([],_)      -> fmli
      (_,FFalse)  -> FFalse
      ([x],TTrue) -> x
      (xs, TTrue) -> And xs
      _           -> And (ss ++ [fmli])

    fmlf = case (es,fmlc) of
      ([],_)       -> fmlc
      ([x],FFalse) -> fNot x
      (xs,FFalse)  -> Or $ map fNot xs
      (_,TTrue)    -> TTrue
      ([x],_)      -> Implies x fmlc
      _            -> Implies (And es) fmlc
  in
    return fmlf

-----------------------------------------------------------------------------

-- | Checks whether a conversion of the signal names to lower case would
-- introduce any clash.

checkLower
  :: String -> Specification -> Either Error ()

checkLower fmt s =
  let
    ids = map ident (inputs s) ++
          map ident (outputs s)
    names = map (idName . (symboltable s !)) ids
    lnames = map (map toLower) names
    znames = zip3 ids names lnames
  in
    checkDouble znames

  where
    ident x = case x of
      SDSingle (y,_) -> y
      SDBus (y,_) _  -> y
      SDEnum (y,_) _ -> y

    checkDouble xs = case xs of
      [] ->  return ()
      [_] -> return ()
      ((i,a,b):(x,c,d):xr) ->
        if b == d
        then errToLower fmt a c $ idPos $ symboltable s ! i
        else checkDouble ((x,c,d) : xr)

-----------------------------------------------------------------------------

-- | Adjust the configuration according to unsupported operators.

adjust
  :: Configuration -> OperatorConfig -> Configuration

adjust c oc =
  c {
    noRelease = noRelease c || unsupported (opRelease oc),
    noWeak = noWeak c || unsupported (opWeak oc),
    noGlobally = noGlobally c || unsupported (opGlobally oc),
    noFinally = noFinally c || unsupported (opFinally oc)
    }

-----------------------------------------------------------------------------
