-----------------------------------------------------------------------------
-- |
-- Module      :  Simplify
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
-- 
-- Linear temporal logic formula simplifcations.
-- 
-----------------------------------------------------------------------------

module Simplify
    ( simplify
    ) where

-----------------------------------------------------------------------------

import Config
    ( Configuration(..)
    )
    
import Data.LTL
    ( Formula(..)
    )
    
import Data.Error
    ( Error
    )  

import Data.Either
    ( partitionEithers
    )

-----------------------------------------------------------------------------

-- | Applies the simplifications specified via the command line arguments.

simplify
  :: Configuration -> Formula -> Either Error Formula

simplify c f =
  let
    f' = simplify' f
  in
   if f == f' then
     return f
   else
     simplify c f'

  where
    simplify' fml = case fml of

      -- Optimization Rules --
      
      Globally TTrue
        | sw || ss || ng || nd                -> TTrue
        | otherwise                        -> Globally TTrue
      Globally FFalse
        | sw || ss || ng || nd                -> FFalse
        | otherwise                        -> Globally FFalse
      Finally TTrue
        | sw || ss || nf || nd                -> TTrue
        | otherwise                        -> Finally TTrue
      Finally FFalse
        | sw || ss || nf || nd                -> FFalse
        | otherwise                        -> Finally FFalse
      Not TTrue
        | sw || ss || nnf                    -> FFalse
        | otherwise                        -> Not TTrue
      Not FFalse
        | sw || ss || nnf                    -> TTrue
        | otherwise                        -> Not FFalse
      Next TTrue
        | sw || ss                          -> TTrue
        | otherwise                        -> Next TTrue
      Next FFalse
        | sw || ss                          -> FFalse
        | otherwise                        -> Next FFalse
      Globally (Globally x)
        | sw || ss || hg || lg || ng || nd      -> simplify' $ Globally x
        | otherwise                        -> Globally $ simplify' $ Globally x
      Finally (Finally x)
        | sw || ss || hf || lf || nf || nd      -> simplify' $ Finally x
        | otherwise                        -> Finally $ simplify' $ Finally x
      Equiv TTrue x
        | sw || ss                          -> simplify' x
        | otherwise                        -> Equiv TTrue $ simplify' x
      Equiv x TTrue
        | sw || ss                          -> simplify' x
        | otherwise                        -> Equiv (simplify' x) TTrue
      Equiv x FFalse
        | sw || ss                          -> simplify' $ Not x
        | otherwise                        -> Equiv (simplify' x) FFalse
      Equiv FFalse x
        | sw || ss                          -> simplify' $ Not x
        | otherwise                        -> Equiv FFalse $ simplify' x
      Implies FFalse x
        | sw || ss                          -> TTrue
        | otherwise                        -> Implies FFalse $ simplify' x
      Implies TTrue x
        | sw || ss                          -> simplify' x
        | otherwise                        -> Implies TTrue $ simplify' x
      Implies x FFalse
        | sw || ss                          -> simplify' $ Not x
        | otherwise                        -> Implies (simplify' x) FFalse
      Implies x TTrue
        | sw || ss                          -> TTrue
        | otherwise                        -> Implies (simplify' x) TTrue
      Not (Not x)
        | sw || ss || nnf                    -> simplify' x
        | otherwise                        -> Not $ Not $ simplify' x
      Not (Next x)
        | ss || ln || nnf                    -> Next $ simplify' $ Not x
        | otherwise                        -> Not $ simplify' $ Next x
      Not (Globally x)
        | ss || nnf                         -> Finally $ simplify' $ Not x
        | otherwise                        -> Not $ simplify' $ Globally x
      Not (Finally x)
        | ss || nnf                         -> Globally $ simplify' $ Not x
        | otherwise                        -> Not $ simplify' $ Finally x
      Not (And xs)
        | ss || nnf                         -> Or $ map (simplify' . Not) xs
        | otherwise                        -> Not $ And $ map simplify' xs
      Not (Or xs)
        | ss || nnf                         -> And $ map (simplify' . Not) xs
        | otherwise                        -> Not $ Or $ map simplify' xs
      Not (Implies x y)
        | ss || nnf                         -> And [simplify' x, simplify' $ Not y]
        | otherwise                        -> Not $ Implies (simplify' x) $ simplify' y
      Not (Equiv (Not x) y)
        | ss || nnf                         -> simplify' $ Equiv x y
        | otherwise                        -> Not $ Equiv (simplify' $ Not x) $ simplify' x
      Not (Equiv x y)
        | ss || nnf                         -> Equiv (simplify' x) $ simplify' $ Not y
        | otherwise                        -> Not $ Equiv (simplify' x) $ simplify' y
      Not (Until x y)
        | ss || nnf                         -> simplify' $ Release (Not x) $ Not y
        | otherwise                        -> Not $ Until (simplify' x) $ simplify' y
      Not (Release x y)
        | ss || nr || nnf                    -> Until (simplify' $ Not x) $ simplify' $ Not y
        | otherwise                        -> Not $ Release (simplify' x) $ simplify' y
      Not (Weak x y)
        | ss || nw || nnf                    -> simplify' $ Not $ Release y $ Or [x,y]
        | otherwise                        -> Not $ Weak (simplify' x) $ simplify' y
      Finally (Next x)
        | ss || ln || hf                     -> simplify' $ Next $ Finally x
        | nf || nd                          -> simplify' $ Until TTrue $ Next x
        | otherwise                        -> Finally $ simplify' $ Next x
      Next (Finally x)
        | (hn && not hf) || (lf && not ln && not ss) -> simplify' $ Finally $ Next x
        | otherwise                        -> Next $ simplify' $ Finally x
      Globally (Next x)
        | ss || ln || hg                     -> simplify' $ Next $ Globally x
        | ng || nd                          -> simplify' $ Release FFalse $ Next x
        | otherwise                        -> Globally $ simplify' $ Next x
      Next (Globally x)
        | (hn && not hg) || (lg && not ln && not ss) -> simplify' $ Globally $ Next x
        | otherwise                        -> Next $ simplify' $ Globally x             
      Until TTrue x
        | ss || (sw && not nf && not nd)          -> simplify' $ Finally x
        | otherwise                        -> Until TTrue $ simplify' x
      Release FFalse x
        | ss || (sw && not ng && not nd)          -> simplify' $ Globally x
        | not nr                             -> Release FFalse $ simplify' x
        | nnf                              -> simplify' $ Weak x FFalse
        | otherwise                        -> simplify' $ Not $ Until TTrue $ Not x
      Until (Next x) (Next y)
        | ss || ln                          -> simplify' $ Next $ Until x y
        | otherwise                        -> Until (simplify' $ Next x) $ simplify' $ Next y
      Next (Until x y)
        | hn                               -> simplify' $ Until (Next x) $ Next y
        | otherwise                        -> Next $ simplify' $ Until x y
      Release (Next x) (Next y)
        | ss || ln                          -> simplify' $ Next $ Release x y
        | not nr                             -> Release (simplify' $ Next x) $ simplify' $ Next y
        | nnf                              -> simplify' $ Weak (Next y) $ And [Next x, Next y]
        | otherwise                        -> simplify' $ Not $ Until (Not $ Next x) (Not $ Next y) 
      Next (Release x y)
        | hn                               -> simplify' $ Release (Next x) $ Next y
        | otherwise                        -> Next $ simplify' $ Release x y
      Weak (Next x) (Next y)
        | ss || ln                          -> simplify' $ Next $ Weak x y
        | nw || nd                          -> simplify' $ Or [Until (Next x) $ Next y, Globally $ Next x]
        | otherwise                        -> Weak (simplify' $ Next x) $ simplify' $ Next y
      Next (Weak x y)
        | hn                               -> simplify' $ Weak (Next x) $ Next y
        | otherwise                        -> Next $ simplify' $ Weak x y
      Until x (Finally y)
        | ss                               -> simplify' $ Finally y
        | otherwise                        -> Until (simplify' x) $ simplify' $ Finally y
      Globally (And xs)
        | hg                               -> simplify' $ And $ map Globally xs
        | ng || nd                          -> simplify' $ Release FFalse $ And xs
        | otherwise                        -> case simplify' $ And xs of
          And ys -> Globally $ And ys
          z      -> simplify' $ Globally z
      Finally (Or xs)
        | hf                               -> simplify' $ Or $ map Finally xs
        | nf || nd                          -> simplify' $ Until TTrue $ Or xs
        | otherwise                        -> case simplify' $ Or xs of
          Or ys -> Finally $ Or ys
          z     -> simplify' $ Finally z
      And []
        | sw || ss                          -> TTrue
        | otherwise                        -> And [] 
      And [Next x]
        | sw || ss || ln                     -> simplify' $ Next x
        | otherwise                        -> And [simplify' $ Next x]
      And [Globally x]
        | sw || ss || lg                     -> simplify' $ Globally x
        | otherwise                        -> And [simplify' $ Globally x]                                       
      And [x]
        | sw || ss                          -> simplify' x
        | otherwise                        -> And [simplify' x]
      And xs
        | not (sw || ss || lg || ln)            -> And $ map simplify' xs
        | otherwise                        ->
          let
            cs | sw || ss   = filter (TTrue /=) $ warpAnd $ map simplify' xs
               | otherwise = xs
          in
            if (sw || ss) && (FFalse `elem` cs) then FFalse
            else case cs of
              []           | sw || ss   -> TTrue
                           | otherwise -> And []
              [Next x]     | sw || ss   -> Next x
                           | ln        -> Next $ And [x]
                           | otherwise -> And [Next x]               
              [Globally x] | sw || ss   -> Globally x
                           | lg        -> Globally $ And [x]
                           | otherwise -> And [Globally x]
              [x]          | sw || ss   -> x
                           | otherwise -> And [x]
              _                      ->
                let
                  (ns, ys) | ln || ss   = partitionEithers $ map splitNext cs
                           | otherwise = ([], cs)
                  (as, zs) | lg || ss   = partitionEithers $ map splitGlobally ys
                           | otherwise = ([], ys)
                in case (ns, as) of
                  ([],[])   -> And zs
                  ([],[y])  -> And $ reverse $ (Globally y) : reverse zs
                  ([],_)    -> And $ reverse $ (Globally $ And as) : reverse zs
                  ([x],[])  -> And $ reverse $ (Next x) : reverse zs
                  ([x],[y]) -> And $ reverse $ (Globally y) : (Next x) : reverse zs
                  ([x],_)   -> And $ reverse $ (Globally $ And as) : (Next x) : reverse zs
                  (_,[])    -> And $ reverse $ (Next $ And ns) : reverse zs
                  (_,[y])   -> And $ reverse $ (Globally y) : (Next $ And ns) : reverse zs
                  (_,_)     -> And $ reverse $ (Globally $ And as) : (Next $ And ns) : reverse zs
      Or []
        | sw || ss                          -> FFalse
        | otherwise                        -> Or []
      Or [Next x]
        | sw || ss || ln                     -> simplify' $ Next x
        | otherwise                        -> Or [simplify' $ Next x]
      Or [Finally x]
        | sw || ss || lf                     -> simplify' $ Finally x
        | otherwise                        -> Or [simplify' $ Finally x]                         
      Or [x]
        | sw || ss                          -> simplify' x
        | otherwise                        -> Or [simplify' x]
      Or xs
        | not (sw || ss || lf || ln)            -> Or $ map simplify' xs
        | otherwise                        -> 
          let
            cs | sw || ss   = filter (FFalse /=) $ warpOr $ map simplify' xs
               | otherwise = xs
          in 
            if (sw || ss) && (TTrue `elem` cs) then TTrue
            else case cs of
               []          | sw || ss   -> FFalse
                           | otherwise -> Or []
               [Next x]    | sw || ss   -> Next x
                           | ln        -> Next $ Or [x]
                           | otherwise -> Or [Next x]               
               [Finally x] | sw || ss   -> Finally x
                           | lf        -> Finally $ Or [x]
                           | otherwise -> Or [Finally x]
               [x]         | sw || ss   -> x
                           | otherwise -> Or [x]
               _                          -> 
                 let
                   (ns, ys) | ln || ss   = partitionEithers $ map splitNext cs
                            | otherwise = ([], cs)
                   (es, zs) | lf || ss   = partitionEithers $ map splitFinally ys
                            | otherwise = ([], ys)
                 in case (ns, es) of
                   ([],[])   -> Or zs
                   ([],[y])  -> Or $ reverse $ (Finally y) : reverse zs
                   ([],_)    -> Or $ reverse $ (Finally $ Or es) : reverse zs
                   ([x],[])  -> Or $ reverse $ (Next x) : reverse zs
                   ([x],[y]) -> Or $ reverse $ (Finally y) : (Next x) : reverse zs
                   ([x],_)   -> Or $ reverse $ (Finally $ Or es) : (Next x) : reverse zs
                   (_,[])    -> Or $ reverse $ (Next $ Or ns) : reverse zs
                   (_,[y])   -> Or $ reverse $ (Finally y) : (Next $ And ns) : reverse zs
                   (_,_)     -> Or $ reverse $ (Finally $ Or es) : (Next $ And ns) : reverse zs

      -- pass through
                        
      Finally x
        | nf || nd                          -> simplify' $ Until TTrue x
        | otherwise                        -> Finally $ simplify' x
      Globally x
        | ng || nd                          -> simplify' $ Release FFalse x
        | otherwise                        -> Globally $ simplify' x
      Release x y
        | not nr                             -> Release (simplify' x) $ simplify' y
        | nnf                              -> simplify' $ Weak y $ And [x,y]
        | otherwise                        -> simplify' $ Not $ Until (Not x) $ Not y
      Weak x y
        | nw || nd                          -> simplify' $ Or [Until x y, Globally x]
        | otherwise                        -> Weak (simplify' x) $ simplify' y
      Equiv x y                            -> Equiv (simplify' x) (simplify' y)              
      Implies x y                          -> Implies (simplify' x) (simplify' y)
      Until x y                            -> Until (simplify' x) (simplify' y)      
      Next x                               -> Next (simplify' x)
      Not (Atomic x)                       -> Not (Atomic x)
      Atomic x                             -> Atomic x
      FFalse                               -> FFalse      
      TTrue                                -> TTrue

    fAnd fml = case fml of
      And x -> x
      _     -> [fml]

    warpAnd = concatMap fAnd

    fOr fml = case fml of
      Or x -> x
      _    -> [fml]
      
    warpOr = concatMap fOr

    splitNext fml = case fml of
      Next x -> Left x
      _      -> Right fml

    splitGlobally fml = case fml of
      Globally x -> Left x
      _          -> Right fml

    splitFinally fml = case fml of
      Finally x -> Left x
      _          -> Right fml      

    nnf = negNormalForm c
    sw = simplifyWeak c
    ss = simplifyStrong c
    nr = noRelease c
    nw = noWeak c 
    ng = noGlobally c 
    nf = noFinally c
    lg = pullGlobally c
    hg = pushGlobally c
    lf = pullFinally c
    hf = pushFinally c
    ln = pullNext c
    hn = pushNext c
    nd = noDerived c

-----------------------------------------------------------------------------
