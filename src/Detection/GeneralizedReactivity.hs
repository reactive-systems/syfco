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
    , sort
    , find  
    )

import Data.Types
    ( Semantics(..)
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
    ( Atomic(..)
    , Formula(..)
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

import Writer.Utils
    ( merge
    )  

import Control.Exception
    ( assert
    )

-----------------------------------------------------------------------------    

import qualified Data.Map.Strict as M

-----------------------------------------------------------------------------

-- | Type of the data structure describing the refusal. 

type Refusal = String

-----------------------------------------------------------------------------

-- | Structure representing a Generalized Reactivity formula.

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
                      
detectGR c s = do
  let
    c' = c {
      simplifyStrong = False,
      noDerived = False,
      noWeak = True,
      noFinally = False,
      noGlobally = False,
      pushGlobally = False,
      pushFinally = False,
      pushNext = True,
      pullFinally = True,
      pullGlobally = True,
      pullNext = False,
      noRelease = True,
      owSemantics = case semantics s of
        SemanticsStrictMoore -> Just SemanticsStrictMealy
        SemanticsMoore       -> Just SemanticsMealy
        _                    -> Nothing
      }

    strict = semantics s `elem` [SemanticsStrictMealy, SemanticsStrictMoore]

  case eval' c' s of
    Left x                    -> Left $ Left x
    Right y -> case quickCheckGR strict y of
      Just f  -> return f
      Nothing -> 
        let
          fml = do
            let c'' = c' {
                  negNormalForm = True,
                  simplifyWeak = True
                  }
                      
            (es,ss,rs,as,is,gs) <- eval c'' s            
            fml' <- merge es ss rs as is gs
            simplify c' $ noImplication $ noEquivalence fml' 
        
        in case fml of
          Left x  -> Left $ Left x
          Right x -> case transformToGR strict x of
            Left z  -> Left $ Right z
            Right z -> return z

  where
    eval' x y = do
      (es,ss,rs,as,is,gs) <- eval x y
      es' <- mapM (simplify x) es
      ss' <- mapM (simplify x) ss
      rs' <- mapM (simplify x) rs
      as' <- mapM (simplify x) as
      is' <- mapM (simplify x) is
      gs' <- mapM (simplify x) gs      

      return (es',ss',rs',as',is',gs')
    
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

-- | Transforms an evaluated, but not combined list of formula to GR,
-- if possible.

quickCheckGR
  :: Bool -> ([Formula],[Formula],[Formula],[Formula],[Formula],[Formula])
  -> Maybe GRFormula

quickCheckGR strict (es,ss,rs,as,is,gs) = do
  mapM_ boolIn es
  mapM_ boolFml ss

  let
    rs'' = case pullG $ fAnd $ concatMap pullG rs of
      Globally (And xs) : ys -> xs ++ ys
      Globally x : ys        -> x : ys
      zs                     -> zs

    (rs',as') = case pullG $ fAnd $ concatMap pullG as of
      Globally (And xs) : ys -> 
        let (ls,os) = partitionEithers $ map boolNextInE xs
        in (rs'' ++ ls, map Globally os ++ ys)
      Globally x : ys -> case boolNextIn x of
        Just ()  -> (x:rs'',ys)
        Nothing -> (rs'', Globally x : ys)
      zs                     -> (rs'',zs)

  case as' of
    [] -> do
      mapM_ boolNextIn rs'

      let
        is'' = case pullG $ fAnd $ concatMap pullG is of
          Globally (And xs) : ys -> xs ++ ys
          Globally x : ys        -> x : ys          
          zs                     -> zs

        (is',gs') = case pullG $ fAnd $ concatMap pullG gs of
          Globally (And xs) : ys -> 
            let (ls,os) = partitionEithers $ map boolNextFmlE xs
            in (is'' ++ ls, map Globally os ++ ys)
          Globally x : ys -> case boolNextFml x of
            Just ()  -> (x:is'',ys)
            Nothing -> (is'', Globally x : ys)               
          zs                     -> (is'',zs)           

      mapM_ boolNextFml is'

      case separateStandardLiveness $
           noImplEquiv $ fAnd gs' of
        Right (ls,[]) -> return
          GRFormula 
            { level = length ls
            , initEnv = es
            , initSys = ss
            , assertEnv = rs'
            , assertSys = is'
            , liveness = ls
            }
        _             -> Nothing
        
    _  -> do
      unless strict Nothing
      
      mapM_ boolNextIn rs'      

      let is' = case pullG $ fAnd $ concatMap pullG is of
            Globally (And xs) : ys -> xs ++ ys
            Globally x : ys        -> x : ys
            zs                     -> zs

      mapM_ boolNextFml is'

      case separateStandardLiveness $
           noImplEquiv $ Implies (fAnd as') (fAnd gs) of
        Right ([x],[]) -> return
          GRFormula 
            { level = 1
            , initEnv = es
            , initSys = ss
            , assertEnv = rs'
            , assertSys = is'
            , liveness = [x]
            }
        _            -> Nothing

  where
    boolIn fml = case fml of
      TTrue            -> return ()
      FFalse           -> return ()
      Atomic (Input _) -> return ()
      Not x            -> boolIn x
      And xs           -> mapM_ boolIn xs
      Or xs            -> mapM_ boolIn xs
      Implies x y      -> mapM_ boolIn [x,y]
      Equiv x y        -> mapM_ boolIn [x,y]
      _                -> Nothing

    boolFml fml = case fml of
      TTrue       -> return ()
      FFalse      -> return ()
      Atomic _    -> return ()
      Not x       -> boolFml x 
      And xs      -> mapM_ boolFml xs
      Or xs       -> mapM_ boolFml xs
      Implies x y -> mapM_ boolFml [x,y]
      Equiv x y   -> mapM_ boolFml [x,y]
      _           -> Nothing

    boolNextIn fml = case fml of
      TTrue             -> return ()
      FFalse            -> return ()
      Atomic _          -> return ()
      Not x             -> boolNextIn x
      And xs            -> mapM_ boolNextIn xs
      Or xs             -> mapM_ boolNextIn xs
      Implies x y       -> mapM_ boolNextIn [x,y]
      Equiv x y         -> mapM_ boolNextIn [x,y]
      Next x            -> boolIn x
      _                 -> Nothing

    boolNextInE fml = case boolNextIn fml of
      Just ()  -> Left fml
      Nothing -> Right fml

    boolNextFml fml = case fml of
      TTrue       -> return ()
      FFalse      -> return ()
      Atomic _    -> return ()
      Not x       -> boolNextFml x      
      And xs      -> mapM_ boolNextFml xs
      Or xs       -> mapM_ boolNextFml xs
      Implies x y -> mapM_ boolNextFml [x,y]
      Equiv x y   -> mapM_ boolNextFml [x,y]
      Next x      -> boolFml x
      _           -> Nothing      

    noImplEquiv fml = case fml of
      TTrue             -> fml
      FFalse            -> fml
      Atomic _          -> fml
      And xs            -> fAnd $ map noImplEquiv xs
      Or xs             -> fOr $ map noImplEquiv xs
      Implies x y       -> fOr [fNot (noImplEquiv x), noImplEquiv y]
      Equiv x y         ->
        let x' = noImplEquiv x
            y' = noImplEquiv y      
        in fOr [fAnd [x',y'], fAnd [fNot x', fNot y']]
      _                 -> fml     
      

    boolNextFmlE fml = case boolNextFml fml of
      Just ()  -> Left fml
      Nothing -> Right fml

    pullG f = case f of
      And xs -> let
          ys = concatMap pullG xs
          (zs,os) = partitionEithers $ map isG ys
        in
          Globally (And zs) : os
      _ -> [f]

    isG f = case f of
      Globally x -> case isG x of
        Left y  -> Left y
        Right y -> Left y
      _          -> Right f      
    

-----------------------------------------------------------------------------        

-- | Transforms an "evaluated" formula to GR, if possible. If it is
-- not possible, the reason for the refusal is returend.

transformToGR
  :: Bool -> Formula -> Either Refusal GRFormula

transformToGR strict fml = do
  -- check that there is no until formula
  noUntil fml
  -- check that there is no next formula on the initial level
  noDirectNext fml
  -- turn the first boolean level of the formula into CNF
  let xs = firstLevelCNF $ pullTogether fml
  -- separate the initial constraints1
  (is,ps,ys) <- separateInitials xs
  -- separate the requirements
  (fs,fr) <- separateRequirements is $ fAnd $ map fOr ys
  -- separate remaining sub-formulas
  (gs,ls,rs) <-
    if strict
    then case separateStandard fr of
      Right (a,b,[]) -> return (a,b,[])
      _              -> separateStrict fr
    else separateStandard fr

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
        
    _  -> errIncompatible $ map fOr rs

-----------------------------------------------------------------------------

separateStrict
  :: Formula
  -> Either Refusal ([Formula],[([Formula],[Formula])],[[Formula]])     

separateStrict fml = let
    -- convert to DNF
    xs = firstLevelDNF $ pullTogether fml
    -- pull globally formulas outwards
    ys = map (pullG . fAnd) xs
    -- ckassify the different elements
    (c1,c2,c3,xr) = foldl classify ([],[],[],[]) ys
  in do
    (gs,ff) <- case c3 of
      []  -> case c2 of
        [] -> return ([],fOr (map (Finally . Globally) c1 ++ map fAnd xr))
        _  -> errIncompatible $
               map (\(x,y) -> fAnd [Finally (Globally x), Globally y]) c2
      [x] -> let
          cs = firstLevelCNF $ fOr $ pullF x
          (fs,zs) = partitionEithers $ map isFL cs
        in if null xr then
             case find ((sort zs /=) . sort . firstLevelCNF . snd) c2 of
               Nothing ->
                 return (map fOr zs,
                         fOr (map (Finally . Globally) (c1 ++ map fst c2) ++
                              map (Globally . Finally) fs))
               Just r  -> errIncompatible [
                           fAnd $ map fOr zs,
                           fAnd $ map fOr $ firstLevelCNF $ snd r
                           ]
           else
             errIncompatible $ map fOr xr
      _   -> assert False undefined
    (ls,rs) <- separateStandardLiveness ff
    return (gs,ls,rs)
    

  where
    pullG f = case f of
      And xs -> let
          ys = concatMap pullG xs
          (gs,rs) = partitionEithers $ map isG ys
        in
          Globally (And gs) : rs
      _ -> [f]

    isG f = case f of
      Globally x -> Left x
      _          -> Right f

    pullF f = case f of
      Or xs -> let
          ys = concatMap pullF xs
          (fs,rs) = partitionEithers $ map isF ys
        in
          Finally (Or fs) : rs
      _ -> [f]

    isF f = case f of
      Finally x -> Left x
      _          -> Right f

    isFL fs = case fs of
      [Finally x] -> Left x
      _           -> Right fs

    classify (a,b,c,d) xs = case xs of
      [Finally (Globally x)]             -> (x:a,b,c,d)
      [Finally (Globally x), Globally y] -> (a,(x,y):b,c,d)
      [Globally y, Finally (Globally x)] -> (a,(x,y):b,c,d)
      [Globally x]                       -> (a,b,x:c,d)
      _                                  -> (a,b,c,xs:d)

-----------------------------------------------------------------------------    

-- | Separation of the invariants and the lifeness constraints under
-- standard semantics.

separateStandard
  :: Formula
  -> Either Refusal ([Formula],[([Formula],[Formula])],[[Formula]])

separateStandard fml = do
  -- convert to CNF
  let xs = firstLevelCNF $ pullTogether fml
  -- separate singleton globally formulas
  (cG,ys) <- separateGlobally xs
  -- check for boolean sub-formulas  
  let (gs,lg) = partition isBooleanNextFormula cG
  -- separate the liveness constriants
  (ls,rs) <- separateStandardLiveness $ fAnd $ map fOr $
              ys ++ map ((: []) . fGlobally) lg
  -- return all sub-formulas
  return (gs,ls,rs)

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

-- | Separates the requirements from the formula and checks the
-- consisitency of the initial conditions.

separateRequirements
  :: [Formula] -> Formula
  -> Either Refusal ([Formula], Formula)

separateRequirements is fml = do
  -- convert formulas to DNF
  let ys = firstLevelDNF $ pullTogether fml
  -- separate the boolean fragment
  (bs,ns) <- separateBoolean ys
    -- check for compatibility
  unless (map (fNot . fAnd) bs == is) $ Left $
    "The initial constraints cannot be refined to fit into the "
    ++ "Generalized Reactivity format."
  -- separate singleton finally formulas
  (cF,fr) <- separateFinally ns 
  -- check for boolean sub-formulas
  let (fs,lf) = partition isBooleanNextFormula cF
  -- check for inputs under next
  mapM_ checkInputsUnderNext fs 
  -- return the result
  return (map fNot fs, fOr $ map fAnd fr ++ map fFinally lf)

  where
    checkInputsUnderNext f = case f of
      Next x ->
        unless (null $ fmlOutputs x) $ Left $
          "GeneralizedReactivity does not allow to constraint "
          ++ "outputs under a transition target of "
          ++ "the environment:\n"
          ++ "  " ++ simplePrint f
      _      -> mapM_ checkInputsUnderNext $ subFormulas f


-----------------------------------------------------------------------------

-- | Separate the GR liveness condition form the formula.

separateStandardLiveness
  :: Formula -> Either Refusal ([([Formula],[Formula])],[[Formula]])

separateStandardLiveness fml =
  let
    -- ensure that we have no messed up formula
    ys = firstLevelCNF $ pullTogether fml
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

    isFGB formula = case formula of
      Finally (Globally f) -> isBooleanFormula f
      Globally (Finally f) -> isBooleanFormula f
      _                    -> False

    sepFG formula = case formula of
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

-- | Returns an error listing all incompatible sub-formulas.    

errIncompatible
  :: [Formula] -> Either Refusal a

errIncompatible xs =
  Left $ "The following sub-formulas cannot be refined "
         ++ "to fit the GeneralizedReactivity requirements:"
         ++ concatMap (\x -> "\n  * " ++ simplePrint x) xs

-----------------------------------------------------------------------------
