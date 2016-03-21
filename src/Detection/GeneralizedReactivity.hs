-----------------------------------------------------------------------------
-- |
-- Module      :  Detection.GeneralizedReactivity
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
-- 
-- Detect whether a specification belongs to the Generalized
-- Reactivity fragment or not.
-- 
-----------------------------------------------------------------------------

module Detection.GeneralizedReactivity
    ( GRFormula(..)
    , Refusal  
    , detectGR
    ) where       

-----------------------------------------------------------------------------

import Simplify
    ( simplify
    )  

import Config
    ( Configuration(..)
    )

import Data.Specification
    ( Specification(..)
    )    

import Data.Either
    ( partitionEithers
    )
    
import Data.List
    ( sortBy
    , partition
    )
    
import Data.Function
    ( on
    )

import Control.Monad
    ( unless  
    )  
    
import Utils
    ( strictSort
    , bucketSort  
    ) 

import Data.LTL
    ( Formula(..)
    , fmlOutputs  
    , subFormulas  
    , isBooleanFormula
    , isBooleanNextFormula 
    , simplePrint  
    , applySub  
    , fFinally  
    , fGlobally  
    , fNot
    , fAnd
    , fOr
    )  

import Data.Error
    ( Error
    )

import Writer.Eval
    ( eval
    )  

import Control.Exception
    ( assert
    )

-----------------------------------------------------------------------------    

import qualified Data.Map.Strict as M

-----------------------------------------------------------------------------

type Refusal = String

-----------------------------------------------------------------------------

data GRFormula =
  GRFormula
  { level :: Int
  , initEnv :: [Formula]
  , initSys :: [Formula]
  , assertEnv :: [Formula]
  , assertSys :: [Formula]
  , liveness :: [([Formula],[Formula])]
  } deriving (Show)

-----------------------------------------------------------------------------             

data TFml =
    TTTrue
  | TFFalse
  | TAtomic Int
  | TAnd [TFml]
  | TOr [TFml]
  deriving (Ord, Eq, Show)  

-----------------------------------------------------------------------------

-- | Detect whether a given specification belongs to the GR
-- fragment of LTL. If so, a 'GRFormula' is returned. Otherwise,
-- either an error occured, or the formula is not in the GR) fragment
-- such that the reason for the refusal is returned.

detectGR
  :: Configuration -> Specification -> Either (Either Error Refusal) GRFormula
                      
detectGR c s = 
  let
    c' = c {
      simplifyWeak = True,
      simplifyStrong = False,
      noDerived = False,
      noWeak = True,
      noFinally = False,
      noGlobally = False,
      negNormalForm = True,
      pushGlobally = False,
      pushFinally = False,
      pushNext = True,
      pullFinally = True,
      pullGlobally = True,
      pullNext = False,
      noRelease = True
      }

    fml = do
      (as,is,gs) <- eval c' s
    
      simplify c' $ noImplication $ noEquivalence $
        Implies (fAnd as) (fAnd [fGlobally $ fAnd is, fAnd gs])
        
  in case fml of
    Left x  -> Left $ Left x
    Right x -> case transformToGR x of
      Left y  -> Left $ Right y
      Right y -> return y

  where
    noImplication fml = case fml of
      Implies x y -> let
          x' = noImplication x
          y' = noImplication y
        in   
          Or [Not x', y']
      _           ->
        applySub noImplication fml           

    noEquivalence fml = case fml of
      Equiv x y -> let 
          x' = noEquivalence x
          y' = noEquivalence y
        in
          And [Implies x' y', Implies y' x']
      _         ->
        applySub noEquivalence fml               

-----------------------------------------------------------------------------

-- | Transforms an "evaluated" formula to GR, if possible. If it is
-- not possible, the reason for the refusal is returend.

transformToGR
  :: Formula -> Either Refusal GRFormula

transformToGR fml = do
  -- check that there is no until formula
  noUntil fml
  -- check that there is no next formula on the initial level
  noDirectNext fml
  -- turn the first boolean level of the formula into CNF
  let xs = firstLevelCNF $ pullTogether fml
  -- separate the initial constraints1
  (is,ps,ys) <- separateInitials xs
  -- separate the invariants
  (fs,gs,zs) <- separateInvariants is ys
  -- separate the liveness constriants
  (ls,rs) <- separateLiveness zs

  case rs of
    [] -> return 
      GRFormula 
        { level = length ls
        , initEnv = is
        , initSys = ps
        , assertEnv = fs
        , assertSys = gs
        , liveness = ls
        }
        
    _  ->
      Left $
        "The following sub-formulas cannot be refined "
        ++ "to fit the GeneralizedReactivity requirements:"
        ++ concatMap (\x -> "\n  * " ++ simplePrint (fOr x)) rs

-----------------------------------------------------------------------------

-- | Check that there is no unil operator inside the formula.    

noUntil
  :: Formula -> Either Refusal ()

noUntil fml = case fml of
  TTrue      -> return ()
  FFalse     -> return ()
  Atomic {}  -> return ()
  Until {}   -> Left $
    "Generalized Reactivity does not allow until sub-formulas:\n" 
    ++ "  " ++ simplePrint fml
  Release {} -> assert False undefined
  Weak {}    -> assert False undefined
  _          -> mapM_ noUntil $ subFormulas fml

-----------------------------------------------------------------------------

-- | Check that there is no next operator not guraded by some finally
-- or globally operator.

noDirectNext
  :: Formula -> Either Refusal () 

noDirectNext fml = case fml of
  TTrue       -> return ()
  FFalse      -> return ()
  Atomic {}   -> return ()
  Globally {} -> return ()
  Finally {}  -> return ()
  Next {}     -> Left $
    "GeneralizedReactivity does not allow next sub-formulas on the initial level:\n"
    ++ "  " ++ simplePrint fml
  _           -> mapM_ noDirectNext $ subFormulas fml

-----------------------------------------------------------------------------

-- | Separate the non-temproal fragment of a formula from the temporal
-- one, where the first boolean level of the formula is either given
-- in DNF or CNF.

separateBoolean
  :: [[Formula]] -> Either Refusal ([[Formula]],[[Formula]])

separateBoolean = return . partition (all isBooleanFormula)

-----------------------------------------------------------------------------

-- | Separate Globally sub-formulas from the formula, given in CNF or DNF.

separateGlobally
  :: [[Formula]] -> Either Refusal ([Formula],[[Formula]])

separateGlobally xs = do
  let (a,b) = partitionEithers $ map separateFormula xs
  return (concat a, b)

  where
    separateFormula ys = case ys of
      [Globally (And zs)] -> Left  zs
      [Globally x]        -> Left [x]
      _                   -> Right ys
      
-----------------------------------------------------------------------------
      
-- | Separate Finally sub-formulas from the formula, given in CNF or DNF.

separateFinally
  :: [[Formula]] -> Either Refusal ([Formula],[[Formula]])

separateFinally xs = do
  let (a,b) = partitionEithers $ map separateFml xs
  return (concat a, b)

  where
    separateFml ys = case ys of
      [Finally (Or zs)] -> Left  zs
      [Finally x]       -> Left [x]
      _                 -> Right ys
      
-----------------------------------------------------------------------------

-- | Separate the initial constraints from the formula.

separateInitials
  :: [[Formula]] -> Either Refusal ([Formula],[Formula],[[Formula]])

separateInitials xs = do
  -- separate the boolean fragment
  (bs,rs) <- separateBoolean xs
  -- convert into DNF
  let ys = firstLevelDNF $ pullTogether $ fAnd $ map fOr bs
  -- separate formulas over inputs from the remaining ones
  let (is,ps) = partitionEithers $ map pureInputFml ys
  -- convert to Generalize Reactivity format
  return (map fNot is, ps, rs)

  where
    pureInputFml ys =
      if null $ fmlOutputs $ fOr ys
      then Left $ fAnd ys
      else Right $ fAnd ys

-----------------------------------------------------------------------------

-- | Separate the invariants from the formula.    

separateInvariants
  :: [Formula] -> [[Formula]]
  -> Either Refusal ([Formula],[Formula],[[Formula]])

separateInvariants is xs = do
  -- convert formulas to DNF
  let ys = firstLevelDNF $ pullTogether $ fAnd $ map fOr xs
  -- separate the boolean fragment
  (bs,ns) <- separateBoolean ys
  -- check for compatibility
  unless (map (fNot . fAnd) bs == is) $ Left $
    "The initial constraints cannot be refined to fit into the "
    ++ "Generalized Reactivity format."
  -- separate singleton finally ormulas
  (cF,fr) <- separateFinally ns
  -- check for boolean sub-formulas
  let (fs,lf) = partition isBooleanNextFormula cF
  -- check for inputs under next
  mapM_ checkInputsUnderNext fs 
  -- convert back to CNF
  let zs = firstLevelCNF $ pullTogether $ fOr $ map fAnd fr ++ map fFinally lf
  -- separate singleton globally formulas
  (cG,rs) <- separateGlobally zs
  -- check for boolean sub-formulas  
  let (gs,lg) = partition isBooleanNextFormula cG
  -- return results
  return (map fNot fs, gs, rs ++ map ((: []) . fGlobally) lg)

  where
    checkInputsUnderNext fml = case fml of
      Next x ->
        unless (null $ fmlOutputs x) $ Left $
          "GeneralizedReactivity does not allow to constraint "
          ++ "outputs under a transition target of "
          ++ "the environment:\n"
          ++ "  " ++ simplePrint fml
      _      -> mapM_ checkInputsUnderNext $ subFormulas fml

-----------------------------------------------------------------------------

-- | Separate the GR liveness condition form the formula.

separateLiveness
  :: [[Formula]] -> Either Refusal ([([Formula],[Formula])],[[Formula]])

separateLiveness xs =
  let
    -- ensure that we have no messed up formula
    ys = firstLevelCNF $ pullTogether $ fAnd $ map fOr xs
    -- check each separate disjunct
    (zs,rs) = partitionEithers $ map classify ys
    -- sort on the first component
    zs' = sortBy (compare `on` fst) zs
    -- join them
    ls = foldl join [] zs'
  in
    return (ls, rs)

  where
    -- join common prefices
    join [] (fs,gs) = [(fs,gs)]
    join ((fs,gs):rs) (fs',gs')
      | fs == fs'  = (fs,gs ++ gs'):rs
      | otherwise = (fs',gs'):(fs,gs):rs
                    
    -- classify all sub-formulas that fit into the GR format
    classify ys
      | not (all isFGB ys) = Right ys
      | otherwise        =
        let (a,b) = partitionEithers $ map sepFG ys
        in Left (strictSort a, strictSort b)


    isFGB fml = case fml of
      Finally (Globally f) -> isBooleanFormula f
      Globally (Finally f) -> isBooleanFormula f
      _                    -> False

    sepFG fml = case fml of
      Finally (Globally f) -> Left $ fNot f
      Globally (Finally f) -> Right f
      _                    -> assert False undefined

-----------------------------------------------------------------------------

-- | Moves finally operators and globally operators inside the formula
-- closer together, i.e., operators of the first level are pushed inside,
-- while operators of the second level are pulled outwards.

pullTogether
  :: Formula -> Formula
    
pullTogether formula = case formula of
  Globally (And ys) -> fAnd $ map (pullTogether . fGlobally) ys
  Globally f        -> fGlobally $ pullUp f
  Finally (Or ys)   -> fOr $ map (pullTogether . fFinally) ys
  Finally f         -> fFinally $ pullUp f
  _                 -> applySub pullTogether formula

  where
    pullUp fml = case fml of
      And []  -> TTrue
      And [y] -> pullUp y
      And ys  ->
        let zs = map pullUp ys
        in if all isGlobally zs
           then fGlobally $ fAnd $ map rmG zs
           else fAnd zs
      Or []   -> FFalse
      Or [y]  -> pullUp y
      Or ys   ->
        let zs = map pullUp ys
        in if all isFinally zs
           then fFinally $ fOr $ map rmF zs
           else fOr zs
      _       -> applySub pullUp fml          

    isGlobally fml = case fml of
      Globally _ -> True
      _          -> False

    isFinally fml = case fml of
      Finally _ -> True
      _         -> False

    rmG fml = case fml of
      Globally f -> f
      _          -> fml

    rmF fml = case fml of
      Finally f -> f
      _         -> fml

-----------------------------------------------------------------------------            

-- | Turns the boolean formula of the first level not containing any
-- temporal operators into DNF. The result is returned as a list of
-- cubes, each consisting of a list of literals, i.e., formulas
-- guarded by a temporal operator.

firstLevelDNF
  :: Formula -> [[Formula]]

firstLevelDNF = firstLevelCNF . swapAndOr

  where
    -- swaps conjunctions and disjunctions on the first, temporal
    -- operator free level.
    swapAndOr f = case f of
      And xs -> Or $ map swapAndOr xs
      Or xs  -> And $ map swapAndOr xs
      _      -> f

-----------------------------------------------------------------------------
    
-- | Turns the boolean formula of the first level not containing any
-- temporal operators into CNF. The result is returned as a list of
-- clauses, each consisting of a list of literals, i.e., formulas
-- guarded by a temporal operator.

firstLevelCNF
  :: Formula -> [[Formula]]

firstLevelCNF formula =
  let
    ys = levelOneAtoms [] formula
    (mm,bb,_) = foldl f (M.empty, M.empty, 0 :: Int) ys
    f (m,b,i) x = case M.lookup x m of
      Just _ -> (m,b,i)
      Nothing -> case M.lookup (fNot x) m of
        Just i' -> (M.insert x (i'+1) m,M.insert (i'+1) x b, i)
        Nothing -> (M.insert x (2*i) m, M.insert (2*i) x b, i+1)
    fml' = replaceLevelOneAtoms mm formula
  in 
    map (map (bb M.!)) $ fCNF fml'
    
  where
    replaceLevelOneAtoms mm fml = case fml of
      TTrue           -> TTTrue
      FFalse          -> TFFalse
      Atomic {}       -> TAtomic $ mm M.! fml
      Not (Atomic {}) -> TAtomic $ mm M.! fml
      Next {}         -> TAtomic $ mm M.! fml
      Finally {}      -> TAtomic $ mm M.! fml
      Globally {}     -> TAtomic $ mm M.! fml
      And xs          -> TAnd $ map (replaceLevelOneAtoms mm) xs
      Or xs           -> TOr $ map (replaceLevelOneAtoms mm) xs
      _               -> assert False undefined
    
    levelOneAtoms a fml = case fml of
      Atomic {}       -> fml : a
      Not (Atomic {}) -> fml : a
      Next {}         -> fml : a
      Finally {}      -> fml : a
      Globally {}     -> fml : a
      And xs          -> foldl levelOneAtoms a xs
      Or xs           -> foldl levelOneAtoms a xs
      _               -> assert False undefined
    
    fCNF fml =
      strictSort $ map strictSort $
      case alreadyCNF $ warp fml of
        Left (us,ps) -> ps ++ concatMap toCNF us
        Right x      -> x

    -- turns non-CNF sub-formulas into CNF
    toCNF (us,ds) = swap $ map (map (ds ++) . fCNF) us

    -- swap and/or alternation in the boolean formula tree

    swap (x:xr) = foldl joinFml x xr
    swap []     = assert False undefined    
    
    joinFml b = simpleFml . concatMap (\zs -> map (zs ++) b)

    simpleFml = filterSupSets . map filternegations

    filternegations = filternegations' [] . bucketSort
    filternegations' a xs = case xs of
      (x : y : xr) 
        | x + 1 == y -> filternegations' a xr
        | otherwise -> filternegations' (x : a) (y : xr)
      (x : xr) -> filternegations' (x : a) xr
      [] -> reverse a 

    -- checks wether the formula is already in CNF and converts
    -- to the list representation, if in CNF. Otherwise the
    -- literals of the clauses not in CNF are separated for the
    -- later conversion
    alreadyCNF fml = case fml of
      TTTrue    -> Right []
      TFFalse   -> Right [[]]
      TAtomic x -> Right [[x]]      
      TAnd xs   -> case partitionEithers $ map pureOr xs of
        ([],zs) -> Right zs
        (ys,zs) -> Left (ys,zs)
      TOr {}    -> alreadyCNF $ TAnd [fml]

    -- separates pure disjunctions of a formula
    pureOr fml = case fml of
      TOr xs    -> case partitionEithers $ map pure xs of
        ([],zs) -> Right zs
        (ys,zs) -> Left (ys,zs)
      TAtomic x -> Right [x]
      _         -> assert False undefined

    -- separates conjunctions of a formula
    pure fml = case fml of
      TAnd {}   -> Left fml
      TAtomic x -> Right x
      _         -> error $ show fml  --assert False undefined

    -- merge all two level And and Or 
    warp fml = case fml of
      TAnd xs -> TAnd $ warpAnd $ map warp xs
      TOr xs  -> TOr $ warpOr $ map warp xs
      _       -> fml

    -- merge two level And
    warpAnd = concatMap wAnd
    wAnd fml = case fml of
      TAnd x -> x
      _      -> [fml]

    -- merge two level Or 
    warpOr = concatMap wOr
    wOr fml = case fml of
      TOr x -> x
      _     -> [fml]    

-----------------------------------------------------------------------------

-- | Removes sets from a set of sets of integers (sets are represented
-- as lists), that are a superset of some other set in the set.

filterSupSets
  :: [[Int]] -> [[Int]]

filterSupSets xs =
  let ys = sortBy (compare `on` length) xs
  in filtersets [] ys

  where
    filtersets a [] = a
    filtersets a (x:xr) =
      filtersets (x:a) (rmsup x [] xr)

    rmsup _ a [] = reverse a
    rmsup x a (y:yr)
      | subset x y  = rmsup x a yr
      | otherwise   = rmsup x (y:a) yr

    subset x y = null $ foldl eatup x y

    eatup [] _ = []
    eatup (x:xr) y
      | x == y     = xr
      | otherwise = x:xr

-----------------------------------------------------------------------------

