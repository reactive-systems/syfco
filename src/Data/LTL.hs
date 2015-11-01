-----------------------------------------------------------------------------
-- |
-- Module      :  Data.LTL
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
-- 
-- Internal representation of Linear Temporal Logic formulas.
-- 
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
    ) where

-----------------------------------------------------------------------------

import qualified Data.Set as S
    ( toList
    , insert
    , empty
    )

-----------------------------------------------------------------------------

-- | Internal representation of an atomic proposition. Each atomic
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

-- | Internal representation of a Linear Temporal Logic formula.

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
  | Globally Formula
  | Finally Formula
  | Until Formula  Formula
  | Release Formula Formula
  | Weak Formula Formula  
  deriving (Eq, Show)

-----------------------------------------------------------------------------

instance Ord Formula where
  compare x y = case (x,y) of
    (Atomic a, Atomic b)       -> compare a b
    (Not a, Not b)             -> compare a b
    (Next a, Next b)           -> compare a b
    (Globally a, Globally b)   -> compare a b
    (Finally a, Finally b)     -> compare a b
    (Implies a b, Implies c d) -> case compare a c of
      EQ -> compare b d
      v  -> v
    (Equiv a b, Equiv c d)     -> case compare a c of
      EQ -> compare b d
      v  -> v
    (Until a b, Until c d)     -> case compare a c of
      EQ -> compare b d
      v  -> v
    (Release a b, Release c d) -> case compare a c of
      EQ -> compare b d
      v  -> v
    (And xs, And ys)           -> case foldl lexord EQ $ zip xs ys of
      EQ -> compare (length xs) (length ys)
      v  -> v
    (Or xs, Or ys)             -> case foldl lexord EQ $ zip xs ys of
      EQ -> compare (length xs) (length ys)
      v  -> v
    _                          -> compare (num x) (num y)  

    where
      num :: Formula -> Int
      
      num f = case f of
        FFalse      -> 0        
        TTrue       -> 1
        Atomic {}   -> 2
        Not {}      -> 3
        Implies {}  -> 4
        Equiv {}    -> 5
        And {}      -> 6
        Or {}       -> 7
        Next {}     -> 8
        Globally {} -> 9
        Finally {}  -> 10
        Until {}    -> 11
        Release {}  -> 12
        Weak {}     -> 13

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
  Not x       -> Not $ f x
  Next x      -> Next $ f x
  Globally x  -> Globally $ f x
  Finally x   -> Finally $ f x
  And xs      -> And $ map f xs
  Or xs       -> Or $ map f xs
  Equiv x y   -> Equiv (f x) (f y)
  Implies x y -> Implies (f x) (f y)
  Until x y   -> Until (f x) (f y)
  Release x y -> Release (f x) (f y)
  Weak x y    -> Weak (f x) (f y)
  _           -> fml

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
    isInput (Output _) =False

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
  TTrue       -> []
  FFalse      -> []
  Atomic _    -> []
  Not x       -> [x]
  Next x      -> [x]
  Globally x  -> [x]
  Finally x   -> [x]
  Implies x y -> [x,y]
  Equiv x y   -> [x,y]
  Until x y   -> [x,y]
  Release x y -> [x,y]
  Weak x y    -> [x,y]
  And xs      -> xs
  Or xs       -> xs
  
-----------------------------------------------------------------------------  


  
