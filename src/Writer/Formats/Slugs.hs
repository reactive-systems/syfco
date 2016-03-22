-----------------------------------------------------------------------------
-- |
-- Module      :  Writer.Formats.Slugs
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
-- 
-- Transforms a specification in GR(1) into the Slugs format.
-- 
-----------------------------------------------------------------------------

module Writer.Formats.Slugs where

-----------------------------------------------------------------------------

import Config

import Utils
import Data.LTL
import Writer.Error
import Data.Specification

import Detection
import Control.Exception

-----------------------------------------------------------------------------

-- | Unbeast format writer.

writeFormat
  :: Configuration -> Specification -> Either Error String

writeFormat c s = do
  case detectGR c s of
    Left v -> case v of
      Left err -> Left err
      Right _  -> errNoGR1 "not in GR(1)" "slugs"
    Right gr
      | level gr > 1 -> errNoGR1 ("in GR(" ++ show (level gr) ++ ")") "slugs"
      | otherwise    -> return $ printSlugs gr
  
  where
    printSlugs gr =
      let
        es = initEnv gr
        ss = initSys gr
        rs = assertEnv gr
        is = assertSys gr
        (le,ls) = head $ liveness gr
        as = es ++ ss ++ rs ++ is ++ le ++ ls
        iv = strictSort $ concatMap fmlInputs as
        ov = strictSort $ concatMap fmlOutputs as
      in
                   "[INPUT]"
        ++ "\n" ++ concatMap (++ "\n") iv
        ++ "\n" ++ "[OUTPUT]"
        ++ "\n" ++ concatMap (++ "\n") ov
        ++ "\n" ++ "[ENV_INIT]"
        ++ "\n" ++ concatMap (++ "\n") (map prFormula es)
        ++ "\n" ++ "[SYS_INIT]"
        ++ "\n" ++ concatMap (++ "\n") (map prFormula ss)
        ++ "\n" ++ "[ENV_TRANS]"
        ++ "\n" ++ concatMap (++ "\n") (map prFormula rs)
        ++ "\n" ++ "[SYS_TRANS]"
        ++ "\n" ++ concatMap (++ "\n") (map prFormula is)
        ++ "\n" ++ "[ENV_LIVENESS]"
        ++ "\n" ++ concatMap (++ "\n") (map prFormula le)
        ++ "\n" ++ "[SYS_LIVENESS]"
        ++ "\n" ++ concatMap (++ "\n") (map prFormula ls)                                
        
    prFormula fml = case fml of
      TTrue                 -> "TRUE"
      FFalse                -> "FALSE"
      Atomic x              -> show x
      Not x                 -> "!" ++ prFormula' x 
      Next (Atomic x)       -> show x ++ "'"
      Next (Not (Atomic x)) -> "!(" ++ show x ++ "')"      
      Next x                -> "X " ++ prFormula' x 
      And []                -> prFormula TTrue
      And [x]               -> prFormula x
      And (x:xr)            -> prFormula' x ++
                              concatMap (\y -> " && " ++ prFormula' y) xr
      Or []                 -> prFormula FFalse
      Or [x]                -> prFormula x
      Or (x:xr)             -> prFormula' x ++ 
                              concatMap (\y -> " || " ++ prFormula' y) xr
      Implies x y           -> prFormula' x ++ " -> " ++ prFormula' y
      Equiv x y             -> prFormula' x ++ " <-> " ++ prFormula' y
      _                     -> assert False undefined

      where
        prFormula' f = case f of
          TTrue                 -> prFormula f
          FFalse                -> prFormula f
          Atomic _              -> prFormula f
          Not _                 -> prFormula f
          Next (Atomic _)       -> prFormula f
          Next (Not (Atomic _)) -> prFormula f          
          _                     -> "(" ++ prFormula f ++ ")"

-----------------------------------------------------------------------------
