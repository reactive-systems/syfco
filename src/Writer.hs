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
    , writeSpecification
    , partition  
    ) where

-----------------------------------------------------------------------------

import Config
    ( Configuration(..)
    )  

import Data.Error
    ( Error
    )
    
import Data.LTL
    ( Formula(..)
    , fmlInputs
    , fmlOutputs  
    )
    
import Data.Specification
    ( Specification
    )

import Writer.Utils
    ( checkLower
    )

import Writer.Error
    ( prError  
    )

import Writer.Eval
    ( eval
    )      
    
import Writer.Formats
    ( WriteFormat(..)
    , needsLower
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

import Control.Monad
    ( when
    )
    
-----------------------------------------------------------------------------        

-- | Creates the contents of a standard partioning file from the lists
-- of input and output signals.

partition
  :: Configuration -> Specification -> IO String

partition c s = case eval c s of
    Left err         -> prError err
    Right (as,is,gs) ->
      let f = And (as ++ is ++ gs)
      in return $ 
         ".inputs" ++ concatMap (' ' :) (fmlInputs f) ++ "\n" ++
         ".outputs" ++ concatMap (' ' :) (fmlOutputs f) ++ "\n"

-----------------------------------------------------------------------------

-- | Unifying function to write a given specification to the desired format.

writeSpecification
  :: Configuration -> Specification -> Either Error String

writeSpecification c s = do
  when (needsLower (outputFormat c)) $
    checkLower (show $ outputFormat c) s
  
  case outputFormat c of
    UTF8    -> writeUtf8 c s
    BASIC   -> writeBasic c s
    WRING   -> writeWring c s 
    LTLXBA  -> writeLtlxba c s 
    PROMELA -> writePromela c s
    UNBEAST -> writeUnbeast c s
    PSL     -> writePsl c s  
  
-----------------------------------------------------------------------------

  
