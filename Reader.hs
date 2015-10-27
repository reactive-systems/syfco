module Reader where

---

import Data.Error
import Data.LookupTable
import Data.Specification

import Reader.Sugar
import Reader.Parser
import Reader.Prescind
import Reader.Bindings
import Reader.InferType

import qualified Reader.Data as RD

import Data.Maybe (fromJust)
import Data.List (zip7)

import qualified Data.IntMap as IM
import qualified Data.Array.IArray as A

---

readSpecification
  :: String -> (Maybe String) -> (Maybe String) ->
     ([String]) -> Either Error Specification

readSpecification str m t ps = do
  s0 <- parseSpecification str m t ps
  s1 <- prescind s0
  s2 <- replaceSugar s1
  s3 <- specBindings s2
  s4 <- inferTypes s3
  return Specification
    { title       = RD.title s4
    , description = RD.description s4
    , semantics   = RD.semantics s4
    , target      = RD.target s4
    , tags        = RD.tags s4
    , parameters  = RD.parameters s4
    , definitions = RD.definitions s4
    , inputs      = RD.inputs s4
    , outputs     = RD.outputs s4
    , assumptions = RD.assumptions s4
    , invariants  = RD.invariants s4
    , guarantees  = RD.guarantees s4
    , lookuptable = ltable s4
    }  

---

ltable
  :: RD.Specification -> LookupTable

ltable s =
  let
    key f =
      if IM.null $ RD.names s then 0 else
        fst $ fst $ fromJust $ f $ RD.names s
    minkey = key IM.minViewWithKey
    maxkey = key IM.maxViewWithKey
    
    is = map fst $ IM.toAscList $ RD.names s
    ns = map snd $ IM.toAscList $ RD.names s
    ps = map snd $ IM.toAscList $ RD.positions s
    as = map snd $ IM.toAscList $ RD.arguments s
    bs = map snd $ IM.toAscList $ RD.bindings s
    ts = map snd $ IM.toAscList $ RD.types s
    ds = map snd $ IM.toAscList $ RD.dependencies s
    
    ys = zip7 is ns ps as bs ts ds
    xs = map (\(a,b,c,d,e ,f,g) -> (a,IdRec b c d e f g)) ys
  in
   A.array (minkey, maxkey) xs

---
