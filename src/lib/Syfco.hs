-----------------------------------------------------------------------------
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
  , QuoteMode(..)
  , Semantics(..)
  , Target(..)
  , Atomic(..)
  , Formula(..)
  , Specification
  , Error
    -- * Configurations
  , defaultCfg
  , update
  , verify
    -- * Specifcations
  , source
  , title
  , description
  , semantics
  , target
  , tags
  , parameters
  , signals
  , inputs
  , outputs
  , symboltable
  , fromTLSF
  , apply
  , applyF
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

import Data.LTL
  ( Atomic(..)
  , Formula(..)
  )

import Data.Error
  ( Error
  )

import Config
  ( Configuration(..)
  , defaultCfg
  , update
  , verify
  )

import Writer.Data
  ( WriteMode(..)
  , QuoteMode(..)
  )

import Writer
  ( WriteFormat(..)
  , apply
  )

import Writer.Eval
  ( signals
  , eval
  )

import Writer.Utils
  ( merge
  )

import Simplify
  ( simplify
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
  ( fromTLSF
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

import Data.Info
  ( version
  )

-----------------------------------------------------------------------------

-- | Returns the parameters of a specification.

parameters
  :: Specification -> [String]

parameters s =
  map (idName . ((S.symboltable s) !) . bIdent) $ S.parameters s

-----------------------------------------------------------------------------

-- | Returns the input signals of a specification using the format as
-- implied by the given configuration.

inputs
  :: Configuration -> Specification -> Either Error [String]

inputs c s = case signals c s of
  Left err     -> Left err
  Right (is,_) -> return is

-----------------------------------------------------------------------------

-- | Returns the ouputs signals of a specification using the format as
-- implied by the given configuration.

outputs
  :: Configuration -> Specification -> Either Error [String]

outputs c s = case signals c s of
  Left err     -> Left err
  Right (_,os) -> return os

-----------------------------------------------------------------------------

-- | Returns the symbol table of a specification in CSV format.

symboltable
  :: Specification -> String

symboltable =
  st2csv . S.symboltable

-----------------------------------------------------------------------------

-- | Same as 'apply', except that the resulting LTL formula is not
-- converted into any specific format, but is returned in the internal
-- data structure.

applyF
  :: Configuration -> Specification -> Either Error Formula

applyF c s = do
  (es,ss,rs,as,is,gs) <- eval c s
  merge es ss rs as is gs >>= simplify c

-----------------------------------------------------------------------------
