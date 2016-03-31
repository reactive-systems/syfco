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

import Data.Enum
    ( EnumDefinition(..)
    )  

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

import Reader.Error
    ( errEnumConflict
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

import qualified Reader.Parser.Data as PD

-----------------------------------------------------------------------------

-- | Reads a specification from a string to the internal 'Specification'
-- data structure.

readSpecification
  :: String -> Either Error Specification

readSpecification str = do
  s0 <- parse str
  mapM_ checkEnum $ PD.enumerations s0
  
  s1 <- abstract s0
  s2 <- replaceSugar s1
  s3 <- specBindings s2
  s4 <- inferTypes s3
  return Specification
    { source       = str
    , title        = RD.title s4
    , description  = RD.description s4
    , semantics    = fst $ RD.semantics s4
    , semanticsP   = snd $ RD.semantics s4
    , target       = fst $ RD.target s4
    , targetP      = snd $ RD.target s4                    
    , tags         = RD.tags s4
    , enumerations = RD.enumerations s4
    , parameters   = RD.parameters s4
    , definitions  = RD.definitions s4
    , inputs       = RD.inputs s4
    , outputs      = RD.outputs s4
    , initially    = RD.initially s4
    , preset       = RD.preset s4
    , requirements = RD.requirements s4                    
    , assumptions  = RD.assumptions s4
    , invariants   = RD.invariants s4
    , guarantees   = RD.guarantees s4
    , symboltable  = symtable s4
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

checkEnum
  :: EnumDefinition String -> Either Error ()

checkEnum e = case eDouble e of
  Nothing      -> return ()
  Just ((m,p),(x,_),(y,_),f) -> errEnumConflict m x y (toStr (eSize e) f) p

  where
    toStr n f = map (toS . f) [0,1..n-1]

    toS (Right ())    = '*'
    toS (Left True)  = '1'
    toS (Left False) = '0'

-----------------------------------------------------------------------------
