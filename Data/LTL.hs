module Data.LTL
       ( Atomic(..)
       , Formula(..)
       , applySub  
       , applyAtomic
       , adjustAtomic  
       , joinAndOr
       , signals
       , fmlInputs
       , fmlOutputs
       , subFormulas
       ) where

---

import qualified Data.Set as S

---

data Atomic =
    Input String
  | Output String
  deriving (Eq)

---

instance Ord Atomic where
  compare x y = case (x,y) of
    (Input _, Output _)  -> LT
    (Output _, Input _)  -> GT
    (Input a, Input b)   -> compare a b
    (Output a, Output b) -> compare a b

---

instance Show Atomic where
  show (Input x)  = x
  show (Output x) = x

---

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

---

instance Ord Formula where
  compare x y = case (x,y) of
    (Atomic a, Atomic b) -> compare a b
    (Not a, Not b) -> compare a b
    (Next a, Next b) -> compare a b
    (Globally a, Globally b) -> compare a b
    (Finally a, Finally b) -> compare a b
    (Implies a b, Implies c d) -> case compare a c of
      EQ -> compare b d
      v  -> v
    (Equiv a b, Equiv c d) -> case compare a c of
      EQ -> compare b d
      v  -> v
    (Until a b, Until c d) -> case compare a c of
      EQ -> compare b d
      v  -> v
    (Release a b, Release c d) -> case compare a c of
      EQ -> compare b d
      v  -> v
    (And xs, And ys) -> case foldl lexord EQ $ zip xs ys of
      EQ -> compare (length xs) (length ys)
      v  -> v
    (Or xs, Or ys) -> case foldl lexord EQ $ zip xs ys of
      EQ -> compare (length xs) (length ys)
      v  -> v
    _ -> compare (topV x) (topV y)  

    where
      topV :: Formula -> Int
      
      topV f = case f of
        FFalse        -> 0        
        TTrue         -> 1
        Atomic {}     -> 2
        Not {}        -> 3
        Implies {}    -> 4
        Equiv {}      -> 5
        And {}        -> 6
        Or {}         -> 7
        Next {}       -> 8
        Globally {}     -> 9
        Finally {} -> 10
        Until {}      -> 11
        Release {}    -> 12
        Weak {}       -> 13

      lexord a (b,c) = case a of
        EQ -> compare b c
        v  -> v
        
---

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

---

applyAtomic
  :: (Atomic -> Formula) -> Formula -> Formula

applyAtomic f fml = case fml of
  Atomic x -> f x
  _        -> applySub (applyAtomic f) fml

---

adjustAtomic
  :: (String -> Atomic) -> Formula -> Formula

adjustAtomic f =  applyAtomic f'
  where
    f' (Output s) = Atomic $ f s
    f' (Input s)  = Atomic $ f s

---

joinAndOr
  :: Formula -> Formula

joinAndOr (And xs) = And $ warpAnd $ map joinAndOr xs 
  where
    ltlAnd (And x) = x
    ltlAnd x = [x]
    warpAnd = concatMap ltlAnd
joinAndOr (Or xs) = Or $ warpOr $ map joinAndOr xs
  where
    ltlOr (Or x) = x
    ltlOr x = [x]
    warpOr = concatMap ltlOr
joinAndOr x = applySub joinAndOr x

---    

signals
  :: Formula -> [Atomic]

signals = S.toList . signals' S.empty 
  where
    signals' a fml = case fml of
      Atomic x -> S.insert x a
      _        -> foldl signals' a $ subFormulas fml

---

fmlInputs
  :: Formula -> [String]

fmlInputs fml = map (\(Input x) -> x) $ filter isInput $ signals fml
  where
    isInput (Input _)  = True
    isInput (Output _) =False

---     

fmlOutputs
  :: Formula -> [String]

fmlOutputs fml = map (\(Output x) -> x) $ filter isOutput $ signals fml
  where
    isOutput (Output _)  = True
    isOutput (Input _) =False     
  
---

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

---  


  
