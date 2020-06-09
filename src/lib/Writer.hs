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

{-# LANGUAGE

    FlexibleContexts
  , RecordWildCards

  #-}

-----------------------------------------------------------------------------

module Writer
  ( WriteFormat(..)
  , apply
  ) where

-----------------------------------------------------------------------------

import Data.Convertible
  ( convert
  )

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
import qualified Writer.Formats.AcaciaSpecs as AcaciaSpecs
import qualified Writer.Formats.Lily as Lily
import qualified Writer.Formats.Ltlxba as Ltlxba
import qualified Writer.Formats.LtlxbaDecomp as LtlxbaDecomp
import qualified Writer.Formats.Ltl as Ltl
import qualified Writer.Formats.Unbeast as Unbeast
import qualified Writer.Formats.Slugs as Slugs
import qualified Writer.Formats.SlugsIn as SlugsIn
import qualified Writer.Formats.Basic as Basic
import qualified Writer.Formats.Full as Full
import qualified Writer.Formats.Psl as Psl
import qualified Writer.Formats.Smv as Smv
import qualified Writer.Formats.Bosy as Bosy
import qualified Writer.Formats.Rabinizer as Rabinizer

-----------------------------------------------------------------------------

-- | Applies the parameters of in the configuration and turns the given
-- specification into the desired target format.

apply
  :: Configuration -> Specification -> Either Error String

apply c@Configuration{..} s = do
  when (needsLower outputFormat) $
    checkLower (convert outputFormat) s

  case outputFormat of
    UTF8        -> Utf8.writeFormat c s
    BASIC       -> Basic.writeFormat c s
    FULL        -> Full.writeFormat c s
    WRING       -> Wring.writeFormat c s
    LTLXBA      -> Ltlxba.writeFormat c s
    LTLXBADECOMP-> LtlxbaDecomp.writeFormat c s
    LTL         -> Ltl.writeFormat c s
    LILY        -> Lily.writeFormat c s
    ACACIA      -> Acacia.writeFormat c s
    ACACIASPECS -> AcaciaSpecs.writeFormat c s
    PROMELA     -> Promela.writeFormat c s
    UNBEAST     -> Unbeast.writeFormat c s
    SLUGS       -> Slugs.writeFormat c s
    SLUGSIN     -> SlugsIn.writeFormat c s
    PSL         -> Psl.writeFormat c s
    SMV         -> Smv.writeFormat c s
    BOSY        -> Bosy.writeFormat c s
    RABINIZER   -> Rabinizer.writeFormat c s

-----------------------------------------------------------------------------
