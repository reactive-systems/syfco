-----------------------------------------------------------------------------
-- |
-- Module      :  Data.LTL
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- Internal representation of Linear Temporal Logic formulas.
--
-----------------------------------------------------------------------------

{-# LANGUAGE

    LambdaCase

  #-}

-----------------------------------------------------------------------------

module Data.LTL
    ( Atomic(..)
    , Formula(..)
    , subFormulas
    , applySub
    , applyAtomic
    , fmlSignals
    , fmlInputs
    , fmlOutputs
    , isBooleanFormula
    , isBooleanNextFormula
    , simplePrint
    , fNot
    , fAnd
    , fOr
    , fGlobally
    , fFinally
    , fHistorically
    , fOnce
    , pastFormula
    ) where

-----------------------------------------------------------------------------

import qualified Data.Set as S
    ( toList
    , insert
    , empty
    )

-----------------------------------------------------------------------------

-- | Representation of an atomic proposition. Each atomic
-- proposition is either an input or output signal.

data Atomic =
    Input String
  | Output String
  deriving (Eq)

-----------------------------------------------------------------------------

instance Ord Atomic where
  compare x y = case (x,y) of
    (Input _, Output _)  -> LT
    (Output _, Input _)  -> GT
    (Input a, Input b)   -> compare a b
    (Output a, Output b) -> compare a b

-----------------------------------------------------------------------------

instance Show Atomic where
  show a = case a of
    Input x  -> x
    Output x -> x

-----------------------------------------------------------------------------

-- | Representation of a linear temporal logic formulas.

data Formula =
    TTrue
  | FFalse
  | Atomic Atomic
  | Not Formula
  | Implies Formula Formula
  | Equiv Formula Formula
  | And [Formula]
  | Or [Formula]
  | Next Formula
  | Previous Formula
  | Globally Formula
  | Finally Formula
  | Historically Formula
  | Once Formula
  | Until Formula  Formula
  | Release Formula Formula
  | Weak Formula Formula
  | Since Formula Formula
  | Triggered Formula Formula
  deriving (Eq, Show)

-----------------------------------------------------------------------------

instance Ord Formula where
  compare x y = case (x,y) of
    (Atomic a, Atomic b)             -> compare a b
    (Not a, Not b)                   -> compare a b
    (Next a, Next b)                 -> compare a b
    (Previous a, Previous b)         -> compare a b
    (Globally a, Globally b)         -> compare a b
    (Finally a, Finally b)           -> compare a b
    (Historically a, Historically b) -> compare a b
    (Once a, Once b)                 -> compare a b
    (Implies a b, Implies c d)       -> case compare a c of
      EQ -> compare b d
      v  -> v
    (Equiv a b, Equiv c d)           -> case compare a c of
      EQ -> compare b d
      v  -> v
    (Until a b, Until c d)           -> case compare a c of
      EQ -> compare b d
      v  -> v
    (Release a b, Release c d)       -> case compare a c of
      EQ -> compare b d
      v  -> v
    (Since a b, Since c d)           -> case compare a c of
      EQ -> compare b d
      v  -> v
    (Triggered a b, Triggered c d)   -> case compare a c of
      EQ -> compare b d
      v  -> v
    (And xs, And ys)                 -> case foldl lexord EQ $ zip xs ys of
      EQ -> compare (length xs) (length ys)
      v  -> v
    (Or xs, Or ys)                   -> case foldl lexord EQ $ zip xs ys of
      EQ -> compare (length xs) (length ys)
      v  -> v
    _                                -> compare (num x) (num y)

    where
      num :: Formula -> Int

      num f = case f of
        FFalse          -> 0
        TTrue           -> 1
        Atomic {}       -> 2
        Not {}          -> 3
        Implies {}      -> 4
        Equiv {}        -> 5
        And {}          -> 6
        Or {}           -> 7
        Next {}         -> 8
        Previous {}     -> 8
        Globally {}     -> 9
        Finally {}      -> 10
        Historically {} -> 10
        Once {}         -> 10
        Until {}        -> 11
        Release {}      -> 12
        Weak {}         -> 13
        Since {}        -> 13
        Triggered {}    -> 13

      lexord a (b,c) = case a of
        EQ -> compare b c
        v  -> v

-----------------------------------------------------------------------------

-- | @applySub f fml@ applies the function @f@ to the direct sub-formulas of
-- @fml@, i.e., every sub-formula under the first operator that appears.
-- If @fml@ is a basic term, the formula remains unchanged.

applySub
  :: (Formula -> Formula) -> Formula -> Formula

applySub f fml = case fml of
  Not x          -> Not $ f x
  Next x         -> Next $ f x
  Previous x     -> Previous $ f x
  Globally x     -> Globally $ f x
  Finally x      -> Finally $ f x
  Historically x -> Historically $ f x
  Once x         -> Once $ f x
  And xs         -> And $ map f xs
  Or xs          -> Or $ map f xs
  Equiv x y      -> Equiv (f x) (f y)
  Implies x y    -> Implies (f x) (f y)
  Until x y      -> Until (f x) (f y)
  Release x y    -> Release (f x) (f y)
  Weak x y       -> Weak (f x) (f y)
  Since x y      -> Since (f x) (f y)
  Triggered x y  -> Triggered (f x) (f y)
  _              -> fml

-----------------------------------------------------------------------------

-- | @applyAtomic f fml@ applies the function @f@ to every atomic
-- proposition of @fml@.

applyAtomic
  :: (Atomic -> Formula) -> Formula -> Formula

applyAtomic f fml = case fml of
  Atomic x -> f x
  _        -> applySub (applyAtomic f) fml

-----------------------------------------------------------------------------

-- | Returns the list of Atomic propositions that appear inside the given
-- formula.

fmlSignals
  :: Formula -> [Atomic]

fmlSignals = S.toList . signals' S.empty
  where
    signals' a fml = case fml of
      Atomic x -> S.insert x a
      _        -> foldl signals' a $ subFormulas fml

-----------------------------------------------------------------------------

-- | Returns the list of input signals that appear inside the given formula.

fmlInputs
  :: Formula -> [String]

fmlInputs fml = map (\(Input x) -> x) $ filter isInput $ fmlSignals fml
  where
    isInput (Input _)  = True
    isInput (Output _) = False

-----------------------------------------------------------------------------

-- | Returns the list of output signals that appear inside the given formula.

fmlOutputs
  :: Formula -> [String]

fmlOutputs fml = map (\(Output x) -> x) $ filter isOutput $ fmlSignals fml
  where
    isOutput (Output _)  = True
    isOutput (Input _) =False

-----------------------------------------------------------------------------

-- | Returns all direct sub-formulas of the given formula, i.e., the formulas
-- that appear under the first operator. If the given formula is a basic
-- term, an empty list is returned.

subFormulas
  :: Formula -> [Formula]

subFormulas fml = case fml of
  TTrue          -> []
  FFalse         -> []
  Atomic _       -> []
  Not x          -> [x]
  Next x         -> [x]
  Previous x     -> [x]
  Globally x     -> [x]
  Finally x      -> [x]
  Historically x -> [x]
  Once x         -> [x]
  Implies x y    -> [x,y]
  Equiv x y      -> [x,y]
  Until x y      -> [x,y]
  Release x y    -> [x,y]
  Weak x y       -> [x,y]
  Since x y      -> [x,y]
  Triggered x y  -> [x,y]
  And xs         -> xs
  Or xs          -> xs

-----------------------------------------------------------------------------

-- | Checks whether a given formula is free of temporal operators.

isBooleanFormula
  :: Formula -> Bool

isBooleanFormula fml = case fml of
  TTrue     -> True
  FFalse    -> True
  Atomic {} -> True
  Not x     -> isBooleanFormula x
  And xs    -> all isBooleanFormula xs
  Or xs     -> all isBooleanFormula xs
  _         -> False

-----------------------------------------------------------------------------

-- | Checks whether a given formula contains 'next' as the only temporal
-- operator.

isBooleanNextFormula
  :: Formula -> Bool

isBooleanNextFormula fml = case fml of
  TTrue     -> True
  FFalse    -> True
  Atomic {} -> True
  Not x     -> isBooleanNextFormula x
  And xs    -> all isBooleanNextFormula xs
  Or xs     -> all isBooleanNextFormula xs
  Next x    -> isBooleanFormula x
  _         -> False

-----------------------------------------------------------------------------

-- | Smart 'And' constructur.

fAnd
  :: [Formula] -> Formula

fAnd xs =
  case filter (/= TTrue) $ warp xs of
    []  -> TTrue
    [x] -> x
    _   -> And xs

  where
    warp = concatMap wAnd
    wAnd fml = case fml of
      And x -> x
      _     -> [fml]

-----------------------------------------------------------------------------

-- | Smart 'Or' constructur.

fOr
  :: [Formula] -> Formula

fOr xs =
  case filter (/= FFalse) $ warp xs of
    []  -> FFalse
    [x] -> x
    _   -> Or xs

  where
    warp = concatMap wOr
    wOr fml = case fml of
      Or x -> x
      _    -> [fml]

-----------------------------------------------------------------------------

-- | Smart 'Not' constructur.

fNot
  :: Formula -> Formula

fNot fml = case fml of
  TTrue           -> FFalse
  FFalse          -> TTrue
  Atomic x        -> Not $ Atomic x
  Not x           -> x
  Next x          -> Next $ fNot x
  Previous x      -> Previous $ fNot x
  Globally x      -> Finally $ fNot x
  Finally x       -> Globally $ fNot x
  Historically x  -> Once $ fNot x
  Once x          -> Historically $ fNot x
  Implies x y     -> And [x, fNot y]
  Equiv (Not x) y -> Equiv x y
  Equiv x (Not y) -> Equiv x y
  Equiv x y       -> Equiv (fNot x) y
  Until x y       -> Release (fNot x) (fNot y)
  Release x y     -> Until (fNot x) (fNot y)
  Weak x y        -> Until (fNot y) (And [fNot x, fNot y])
  Since x y       -> Triggered (fNot x) (fNot y)
  Triggered x y   -> Since (fNot x) (fNot y)
  And xs          -> Or $ map fNot xs
  Or xs           -> And $ map fNot xs

-----------------------------------------------------------------------------

-- | Smart 'Globally' constructur.

fGlobally
  :: Formula -> Formula

fGlobally fml = case fml of
  TTrue       -> TTrue
  FFalse      -> FFalse
  Globally {} -> fml
  _           -> Globally fml

-----------------------------------------------------------------------------

-- | Smart 'Globally' constructur.

fFinally
  :: Formula -> Formula

fFinally fml = case fml of
  TTrue      -> TTrue
  FFalse     -> FFalse
  Finally {} -> fml
  _          -> Finally fml

-----------------------------------------------------------------------------

-- | Smart 'Historically' constructur.

fHistorically
  :: Formula -> Formula

fHistorically fml = case fml of
  TTrue          -> TTrue
  FFalse         -> FFalse
  Historically _ -> fml
  _              -> Historically fml

-----------------------------------------------------------------------------

-- | Smart 'Once' constructur.

fOnce
  :: Formula -> Formula

fOnce fml = case fml of
  TTrue   -> TTrue
  FFalse  -> FFalse
  Once {} -> fml
  _       -> Once fml

-----------------------------------------------------------------------------

-- | Simple printing.

simplePrint
  :: Formula -> String

simplePrint fml = case fml of
  TTrue           -> "true"
  FFalse          -> "false"
  Atomic x        -> show x
  Not x           -> '!' : simplePrint x
  Next x          -> 'X' : ' ' : simplePrint x
  Previous x      -> 'Y' : ' ' : simplePrint x
  Globally x      -> 'G' : ' ' : simplePrint x
  Finally x       -> 'F' : ' ' : simplePrint x
  Historically x  -> 'H' : ' ' : simplePrint x
  Once x          -> 'O' : ' ' : simplePrint x
  Implies x y     -> "(" ++ simplePrint x ++ " -> " ++ simplePrint y ++ ")"
  Equiv x y       -> "(" ++ simplePrint x ++ " <-> " ++ simplePrint y ++ ")"
  Until x y       -> "(" ++ simplePrint x ++ " U " ++ simplePrint y ++ ")"
  Release x y     -> "(" ++ simplePrint x ++ " R " ++ simplePrint y ++ ")"
  Weak x y        -> "(" ++ simplePrint x ++ " W " ++ simplePrint y ++ ")"
  Since x y       -> "(" ++ simplePrint x ++ " S " ++ simplePrint y ++ ")"
  Triggered x y   -> "(" ++ simplePrint x ++ " T " ++ simplePrint y ++ ")"
  And []          -> simplePrint TTrue
  And [x]         -> simplePrint x
  And (x:xr)      -> "(" ++ simplePrint x ++
                     concatMap ((" && " ++) . simplePrint) xr ++ ")"
  Or []           -> simplePrint FFalse
  Or (x:xr)       -> "(" ++ simplePrint x ++
                     concatMap ((" || " ++) . simplePrint) xr ++ ")"

-----------------------------------------------------------------------------

-- | Checks whether the formula contains past operators.

pastFormula
  :: Formula -> Bool

pastFormula = \case
  TTrue           -> False
  FFalse          -> False
  Atomic _        -> False
  Not x           -> pastFormula x
  Next x          -> pastFormula x
  Previous {}     -> True
  Globally x      -> pastFormula x
  Finally x       -> pastFormula x
  Historically {} -> True
  Once {}         -> True
  Implies x y     -> pastFormula x || pastFormula y
  Equiv x y       -> pastFormula x || pastFormula y
  Until x y       -> pastFormula x || pastFormula y
  Release x y     -> pastFormula x || pastFormula y
  Weak x y        -> pastFormula x || pastFormula y
  Since {}        -> True
  Triggered {}    -> True
  And []          -> False
  And (x:xr)      -> pastFormula x || pastFormula (And xr)
  Or []           -> False
  Or (x:xr)       -> pastFormula x || pastFormula (Or xr)

-----------------------------------------------------------------------------
