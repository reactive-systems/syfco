----------------------------------------------------------------------------
-- |
-- Module      :  Syfco
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- Syfco Library Interface.
--
-----------------------------------------------------------------------------

{-# LANGUAGE

    RecordWildCards

  #-}

-----------------------------------------------------------------------------

module Syfco
  ( -- * Data Structures
    Configuration(..)
  , WriteFormat(..)
  , WriteMode(..)
  , Semantics(..)
  , Target(..)
  , Specification
  , Error
    -- * Configurations
  , defaultCfg
  , readCfg
  , writeCfg
  , checkCfg
    -- * Specifcations
  , source
  , title
  , description
  , semantics
  , target
  , tags
  , parameters
  , inputs
  , outputs
  , symboltable
  , readSpecification
  , writeSpecification
  , writePartition
    -- * Fragment Detection
  , checkGR
    -- * Meta Information
  , version
  ) where

-----------------------------------------------------------------------------

import Data.Types
  ( Semantics(..)
  , Target(..)
  )

import Data.Error
  ( Error
  )

import Config
  ( Configuration(..)
  , defaultCfg
  , readCfg
  , writeCfg
  , checkCfg
  )

import Writer.Data
  ( WriteMode(..)
  )

import Writer
  ( WriteFormat(..)
  , writeSpecification
  , writePartition
  )

import Data.Specification
  ( Specification
  , source
  , title
  , description
  , semantics
  , target
  , tags
  )

import qualified Data.Specification as S
  ( symboltable
  , parameters
  )

import Reader
  ( readSpecification
  )

import Detection
  ( checkGR
  )

import Data.Binding
  ( BindExpr(..)
  )

import Data.SymbolTable
  ( IdRec(..)
  , st2csv
  )

import Data.Array
  ( (!)
  )

import Writer.Eval
  ( eval
  )

import Data.LTL
  ( Formula(..)
  , fmlInputs
  , fmlOutputs
  )

import Data.Info
  ( version
  )

-----------------------------------------------------------------------------

-- | The parameters of the given specification.

parameters
  :: Specification -> [String]

parameters s =
  map (idName . ((S.symboltable s) !) . bIdent) $ S.parameters s

-----------------------------------------------------------------------------

-- | The input signals of the given specification.

inputs
  :: Configuration -> Specification -> Either Error [String]

inputs c s = case eval c s of
  Left err                  -> Left err
  Right (es,ss,rs,as,is,gs) ->
    return $ fmlInputs $ And $ es ++ ss ++ rs ++ as ++ is ++ gs

-----------------------------------------------------------------------------

-- | The input signals of the given specification.

outputs
  :: Configuration -> Specification -> Either Error [String]

outputs c s = case eval c s of
  Left err                  -> Left err
  Right (es,ss,rs,as,is,gs) ->
    return $ fmlOutputs $ And $ es ++ ss ++ rs ++ as ++ is ++ gs

-----------------------------------------------------------------------------

-- | Returns the symbol table of the specification as CSV file.

symboltable
  :: Specification -> String

symboltable =
  st2csv . S.symboltable

-----------------------------------------------------------------------------
