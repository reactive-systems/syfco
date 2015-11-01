-----------------------------------------------------------------------------
-- |
-- Module      :  Reader
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
-- 
-- The module reads a specification to the internal format.
-- 
-----------------------------------------------------------------------------

module Reader
     ( readSpecification
     ) where

-----------------------------------------------------------------------------

import Data.Error
    ( Error
    )
    
import Data.SymbolTable
    ( SymbolTable
    , IdRec(..)
    )
    
import Data.Specification
    ( Specification(..)
    )  

import Reader.Sugar
    ( replaceSugar
    )
    
import Reader.Parser
    ( parse
    )  
 
import Reader.Bindings
    ( specBindings
    )
    
import Reader.InferType
    ( inferTypes
    )
    
import Reader.Abstraction
    ( abstract
    )  

import Data.Maybe
    ( fromJust
    )
    
import Data.List
    ( zip7
    )

import qualified Data.IntMap as IM

import qualified Data.Array.IArray as A

import qualified Reader.Data as RD

-----------------------------------------------------------------------------

-- | Reads a specification from a string to the internal 'Specification'
-- data structure.

readSpecification
  :: String -> (Maybe String) -> (Maybe String) ->
     ([String]) -> Either Error Specification

readSpecification str m t ps = do
  s0 <- parse str m t ps
  s1 <- abstract s0
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
    , symboltable = symtable s4
    }  

-----------------------------------------------------------------------------

symtable
  :: RD.Specification -> SymbolTable

symtable s =
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

-----------------------------------------------------------------------------
