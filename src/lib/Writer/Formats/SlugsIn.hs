-----------------------------------------------------------------------------
-- |
-- Module      :  Writer.Formats.SlugsIn
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Ioannis Filippidis (jfilippidis@gmail.com)
--                Felix Klein (klein@react.uni-saarland.de)
--
-- Translates GR(1) specification to SlugsIn syntax.
--
-----------------------------------------------------------------------------

module Writer.Formats.SlugsIn where

-----------------------------------------------------------------------------

import Config

import Data.LTL
import Writer.Eval
import Writer.Error
import Data.Specification

import Detection
import Control.Exception

-----------------------------------------------------------------------------

-- | SlugsIn format writer.

writeFormat
  :: Configuration -> Specification -> Either Error String

writeFormat c s =
  case detectGR c s of
    Left v -> case v of
      Left err -> Left err
      Right _  -> errNoGR1 "not in GR(1)" "slugsin"
    Right gr
      | level gr > 1 -> errNoGR1 ("in GR(" ++ show (level gr) ++ ")") "slugsin"
      | otherwise    -> printSlugs gr

  where
    printSlugs gr = do
      let
        es = initEnv gr
        ss = initSys gr
        rs = assertEnv gr
        is = assertSys gr
        (le,ls) = case liveness gr of
          []  -> ([],[])
          x:_ -> x

      (iv,ov) <- signals c s

      return $ "[INPUT]"
        ++ "\n" ++ unlines iv
        ++ "\n" ++ "[OUTPUT]"
        ++ "\n" ++ unlines ov
        ++ (if null es then "" else
             "\n" ++ "[ENV_INIT]" ++
             "\n" ++ unlines (map prFormula es))
        ++ (if null ss then "" else
             "\n" ++ "[SYS_INIT]" ++
             "\n" ++ unlines (map prFormula ss))
        ++ (if null rs then "" else
              "\n" ++ "[ENV_TRANS]" ++
              "\n" ++ unlines (map prFormula rs))
        ++ (if null is then "" else
              "\n" ++ "[SYS_TRANS]" ++
              "\n" ++ unlines (map prFormula is))
        ++ (if null le then "" else
              "\n" ++ "[ENV_LIVENESS]" ++
              "\n" ++ unlines (map prFormula le))
        ++ (if null ls then "" else
             "\n" ++ "[SYS_LIVENESS]" ++
             "\n" ++ unlines (map prFormula ls))

    prFormula fml = case fml of
      TTrue                 -> " 1 "
      FFalse                -> " 0 "
      Atomic x              -> " " ++ show x ++ " "
      Not x                 -> "! " ++ prFormula x
      Next x                -> prFormula' x
      And []                -> prFormula TTrue
      And [x]               -> prFormula x
      And (x:xr)            -> concatMap (\_ -> " & ") xr ++
                               prFormula x ++
                               concatMap (\y -> prFormula y) xr
      Or []                 -> prFormula FFalse
      Or [x]                -> prFormula x
      Or (x:xr)             -> concatMap (\_ -> " | ") xr ++
                               prFormula x ++
                               concatMap (\y -> prFormula y) xr
      Implies x y           -> " | ! " ++
                               prFormula x ++
                               prFormula y
      Equiv x y             -> " ! ^ " ++
                               prFormula x ++
                               prFormula y
      _                     -> assert False undefined


      where prFormula' f = case f of
              TTrue                 -> " 1 "
              FFalse                -> " 0 "
              Atomic x              -> " " ++ show x ++ "' "
              Not x                 -> "! " ++ prFormula' x
              Next {}               -> assert False undefined
              And []                -> prFormula' TTrue
              And [x]               -> prFormula' x
              And (x:xr)            -> concatMap (\_ -> " & ") xr ++
                                       prFormula' x ++
                                       concatMap (\y -> prFormula' y) xr
              Or []                 -> prFormula' FFalse
              Or [x]                -> prFormula' x
              Or (x:xr)             -> concatMap (\_ -> " | ") xr ++
                                       prFormula' x ++
                                       concatMap (\y -> prFormula' y) xr
              Implies x y           -> " | ! " ++
                                       prFormula' x ++
                                       prFormula' y
              Equiv x y             -> " ! ^ " ++
                                       prFormula' x ++
                                       prFormula' y
              _                     -> assert False undefined


-----------------------------------------------------------------------------
