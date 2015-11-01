-----------------------------------------------------------------------------
-- |
-- Module      :  Writer
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
-- 
-- The module provides the different writers, linked via
-- 'writeSpecification'.
-- 
-----------------------------------------------------------------------------

module Writer
    ( WriteFormat(..)
    , WriteContents(..)  
    , writeSpecification
    ) where

-----------------------------------------------------------------------------

import Config
    ( Configuration(..)
    )  

import Data.Error
    ( Error
    )
    
import Data.Specification
    ( Specification
    )  

import Writer.Data
    ( WriteContents(..)
    )
    
import Writer.Formats
    ( WriteFormat(..)
    ) 

import Writer.Formats.Utf8
    ( writeUtf8
    )
    
import Writer.Formats.Wring
    ( writeWring
    )
    
import Writer.Formats.Promela
    ( writePromela
    )
    
import Writer.Formats.Ltlxba
    ( writeLtlxba
    )
    
import Writer.Formats.Unbeast
    ( writeUnbeast
    )
    
import Writer.Formats.Basic
    ( writeBasic
    )
    
import Writer.Formats.Psl
    ( writePsl
    )

-----------------------------------------------------------------------------

-- | Unifying function to write a given specification to the desired format.

writeSpecification
  :: Configuration -> Specification -> Either Error WriteContents

writeSpecification c s = case outputFormat c of
  UTF8    -> writeUtf8 c s
  SHORT   -> writeBasic c s
  WRING   -> writeWring c s 
  LTLXBA  -> writeLtlxba c s 
  PROMELA -> writePromela c s
  UNBEAST -> writeUnbeast c s
  PSL     -> writePsl c s  

-----------------------------------------------------------------------------

  
