-----------------------------------------------------------------------------
-- |
-- Module      :  Reader
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- The module reads a specification to the internal format.
--
-----------------------------------------------------------------------------

{-# LANGUAGE

    RecordWildCards

  #-}

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
  ( null
  , toAscList
  , minViewWithKey
  , maxViewWithKey
  )

import qualified Data.Array.IArray as A
  ( array
  )

import qualified Reader.Data as RD
  ( Specification(..)
  )

import qualified Reader.Parser.Data as PD
  ( enumerations
  )

import Debug.Trace

-----------------------------------------------------------------------------

-- | Reads a specification from a string to the internal 'Specification'
-- data structure.

readSpecification
  :: String -> Either Error Specification

readSpecification str =
  -- parse the input
  parse str >>=

  -- replace variable names by a unique identifier
  abstract >>=

  -- replace syntactic sugar constructs for later converison
  replaceSugar >>=

  -- retrieve the bindings of expression variables
  specBindings >>=

  -- infer types and typecheck
  inferTypes >>=

  -- lift reader specification to global specification
  \(s @ RD.Specification {..}) -> return
    Specification
      { source         = str
      , title          = fst title
      , titlePos       = snd title
      , description    = fst description
      , descriptionPos = snd description
      , semantics      = fst semantics
      , semanticsPos   = snd semantics
      , target         = fst target
      , targetPos      = snd target
      , tags           = map fst $ tags
      , tagsPos        = map snd $ tags
      , enumerations   = enumerations
      , parameters     = parameters
      , definitions    = definitions
      , inputs         = inputs
      , outputs        = outputs
      , initially      = initially
      , preset         = preset
      , requirements   = requirements
      , assumptions    = assumptions
      , invariants     = invariants
      , guarantees     = guarantees
      , symboltable    = symtable s
      }

-----------------------------------------------------------------------------

symtable
  :: RD.Specification -> SymbolTable

symtable (RD.Specification {..}) =
  let
    minkey = key IM.minViewWithKey
    maxkey = key IM.maxViewWithKey

    is = map fst $ IM.toAscList names
    ns = map snd $ IM.toAscList names
    ps = map snd $ IM.toAscList positions
    as = map snd $ IM.toAscList arguments
    bs = map snd $ IM.toAscList bindings
    ts = map snd $ IM.toAscList types
    ds = map snd $ IM.toAscList dependencies

    ys = zip7 is ns ps as bs ts ds
    xs = map (\(a,b,c,d,e,f,g) -> (a,IdRec b c d e f g)) ys
  in
    A.array (minkey, maxkey) xs

  where
    key f
      | IM.null names = 0
      | otherwise     =
          fst $ fst $ fromJust $ f names

-----------------------------------------------------------------------------
