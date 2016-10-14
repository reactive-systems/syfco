-----------------------------------------------------------------------------
-- |
-- Module      :  Info
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- Several printers to report information back to the user.
--
-----------------------------------------------------------------------------

module Info
  ( prTitle
  , prDescription
  , prSemantics
  , prTarget
  , prTags
  , prInfo
  , prParameters
  , prInputs
  , prOutputs
  , prVersion
  , prHelp
  , prReadme
  , prReadmeMd
  ) where

-----------------------------------------------------------------------------

import Data.Info
  ( toolName
  , toolVersion
  , helpMsg
  , readmeMsg
  , readmeMdMsg
  )

import Data.Array
  ( (!)
  )

import Data.Types
  ( Semantics(..)
  , Target(..)
  )

import Config
  ( Configuration(..)
  )

import Data.LTL
  ( Formula(..)
  , fmlInputs
  , fmlOutputs
  )

import Data.Error
  ( prError
  )

import Data.Binding
  ( BindExpr(..)
  )

import Data.SymbolTable
  ( IdRec(..)
  )

import Data.Specification
  ( Specification(..)
  )

import Writer.Eval
  ( eval
  )

import Control.Monad
  ( unless
  )

import System.Environment
  ( getProgName
  )

-----------------------------------------------------------------------------

-- | Prints the title of the given specification.

prTitle
  :: Specification -> IO ()

prTitle s =
  putStrLn $ title s

-----------------------------------------------------------------------------

-- | Prints the description of the given specification.

prDescription
  :: Specification -> IO ()

prDescription s =
  putStrLn $ description s

-----------------------------------------------------------------------------

-- | Prints the semantics of the given specification.

prSemantics
  :: Specification -> IO ()

prSemantics s =
  putStrLn $ case semantics s of
    SemanticsMealy -> "Mealy"
    SemanticsMoore -> "Moore"
    SemanticsStrictMealy -> "Strict,Mealy"
    SemanticsStrictMoore -> "Strict,Moore"

-----------------------------------------------------------------------------

-- | Prints the target of the given specification.

prTarget
  :: Specification -> IO ()

prTarget s =
  putStrLn $ case target s of
    TargetMealy -> "Mealy"
    TargetMoore -> "Moore"

-----------------------------------------------------------------------------

-- | Prints the tag list of the given specification.

prTags
  :: Specification -> IO ()

prTags s = case tags s of
  [] -> return ()
  xs -> putStrLn $ head xs ++ concatMap ((:) ' ' . (:) ',') (tail xs)

-----------------------------------------------------------------------------

-- | Prints the parameters of the given specification.

prParameters
  :: Specification -> IO ()

prParameters s =
  let
    xs = map bIdent $ parameters s
    ys = map (idName . (symboltable s !)) xs
  in
    putStrLn $
    if null ys then ""
    else head ys ++ concatMap ((:) ',' . (:) ' ') (tail ys)

-----------------------------------------------------------------------------

-- | Prints the input signals of the given specification.

prInputs
  :: Configuration -> Specification -> IO ()

prInputs c s = case eval c s of
  Left err         -> prError err
  Right (es,ss,rs,as,is,gs) -> putStrLn $
    case fmlInputs $ And $ es ++ ss ++ rs ++ as ++ is ++ gs of
      (x:xr) -> x ++ concatMap ((:) ',' . (:) ' ') xr
      []     -> ""

-----------------------------------------------------------------------------

-- | Prints the output signals of the given specification.

prOutputs
  :: Configuration -> Specification -> IO ()

prOutputs c s = case eval c s  of
  Left err         -> prError err
  Right (es,ss,rs,as,is,gs) -> putStrLn $
    case fmlOutputs $ And $ es ++ ss ++ rs ++ as ++ is ++ gs of
      (x:xr) -> x ++ concatMap ((:) ',' . (:) ' ') xr
      []     -> ""

-----------------------------------------------------------------------------

-- | Prints the complete INFO section of the given specification.

prInfo
  :: Specification -> IO ()

prInfo s = do
  putStrLn $ "Title:         \"" ++ title s ++ "\""
  putStrLn $ "Description:   \"" ++ description s ++ "\""
  putStr "Semantics:     "
  prSemantics s
  putStr "Target:        "
  prTarget s
  unless (null $ tags s) $ do
    putStr "Tags:          "
    prTags s

-----------------------------------------------------------------------------

-- | Prints the version and the program name.

prVersion
  :: IO ()

prVersion = do
  putStrLn $ "SyFCo (v" ++ toolVersion ++ ")"
  putStrLn "The Synthesis Format Converter"

-----------------------------------------------------------------------------

-- | Prints the help of the program.

prHelp
  :: IO ()

prHelp = putStr helpMsg

-----------------------------------------------------------------------------

-- | Prints the content of the README file.

prReadme
  :: IO ()

prReadme = putStr readmeMsg

-----------------------------------------------------------------------------

-- | Prints the content of the README.md file.

prReadmeMd
  :: IO ()

prReadmeMd = putStr readmeMdMsg

-----------------------------------------------------------------------------
