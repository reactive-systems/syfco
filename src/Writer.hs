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
    ( evalSignals
    )      
    
import Writer.Formats
    ( WriteFormat(..)
    , needsLower
    )
    
import Control.Monad
    ( when
    )

-----------------------------------------------------------------------------    

import qualified Writer.Formats.Utf8 as Utf8
import qualified Writer.Formats.Wring as Wring
import qualified Writer.Formats.Promela as Promela
import qualified Writer.Formats.Acacia as Acacia
import qualified Writer.Formats.Lily as Lily
import qualified Writer.Formats.Ltlxba as Ltlxba
import qualified Writer.Formats.Unbeast as Unbeast
import qualified Writer.Formats.Slugs as Slugs
import qualified Writer.Formats.SlugsIn as SlugsIn
import qualified Writer.Formats.Basic as Basic
import qualified Writer.Formats.Full as Full
import qualified Writer.Formats.Psl as Psl

-----------------------------------------------------------------------------

-- | Creates the contents of a standard partioning file from the lists
-- of input and output signals.

partition
  :: Configuration -> Specification -> IO String

partition c s = case evalSignals c s of
    Left err      -> prError err
    Right (is,os) ->
      return $ 
        ".inputs" ++ concatMap (' ' :) is ++ "\n" ++
        ".outputs" ++ concatMap (' ' :) os ++ "\n"

-----------------------------------------------------------------------------

-- | Unifying function to write a given specification to the desired format.

writeSpecification
  :: Configuration -> Specification -> Either Error String

writeSpecification c s = do
  when (needsLower (outputFormat c)) $
    checkLower (show $ outputFormat c) s
  
  case outputFormat c of
    UTF8    -> Utf8.writeFormat c s 
    BASIC   -> Basic.writeFormat c s
    FULL    -> Full.writeFormat c s    
    WRING   -> Wring.writeFormat c s 
    LTLXBA  -> Ltlxba.writeFormat c s
    LILY    -> Lily.writeFormat c s     
    ACACIA  -> Acacia.writeFormat c s 
    PROMELA -> Promela.writeFormat c s 
    UNBEAST -> Unbeast.writeFormat c s
    SLUGS   -> Slugs.writeFormat c s     
    SLUGSIN -> SlugsIn.writeFormat c s
    PSL     -> Psl.writeFormat c s 
  
-----------------------------------------------------------------------------

  
