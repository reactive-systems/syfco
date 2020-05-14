-----------------------------------------------------------------------------
-- |
-- Module      :  Arguments
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- Argument parser.
--
-----------------------------------------------------------------------------

{-# LANGUAGE

    LambdaCase
  , RecordWildCards

  #-}

-----------------------------------------------------------------------------

module Arguments
  ( parseArguments
  ) where

-----------------------------------------------------------------------------

import Syfco
  ( Configuration(..)
  , WriteMode(..)
  , QuoteMode(..)
  , defaultCfg
  , verify
  , update
  )

import Data.Convertible
  ( safeConvert
  )

import Info
  ( prError
  )

import System.Directory
  ( doesFileExist
  )

import Control.Monad
  ( void
  , unless
  )

import Text.Parsec.String
  ( Parser
  )

import Text.Parsec
  ( parse
  , letter
  )

import Text.Parsec
  ( (<|>)
  , char
  , many1
  , digit
  , alphaNum
  , eof
  )

import Text.Parsec.Token
  ( GenLanguageDef(..)
  , makeTokenParser
  , identifier
  )

import Text.Parsec.Language
  ( emptyDef
  )

-----------------------------------------------------------------------------

data Args a = None a | Single a

-----------------------------------------------------------------------------

-- | Argument parser, which reads the given command line arguments to
-- the internal configuration and checks whether the given
-- combinations are realizable.

parseArguments
  :: [String] -> IO Configuration

parseArguments args = do
  c <- traverse defaultCfg args

  case verify c of
    Left err -> prError $ show err
    _        -> return c

  where
    traverse c = \case
      x:y:xr -> do
        r <- parseArgument c x (Just y)
        case r of
          Single c'-> traverse c' xr
          None c'  -> traverse c' (y:xr)
      [x]    -> do
        r <- parseArgument c x Nothing
        case r of
          None c'   -> return c'
          Single c' -> return c'
      []     -> return c

    parseArgument c arg next = case arg of
      "-o"                       -> case next of
        Just x  -> return $ Single $ c { outputFile = Just x }
        Nothing -> argsError "\"-o\": No output file"
      "--output"                 -> case next of
        Nothing -> argsError "\"--output\": No output file"
        _       -> parseArgument c "-o" next
      "-r"                       -> case next of
        Just file -> do
          exists <- doesFileExist file
          unless exists $ argsError $ "File does not exist: " ++ file
          fmap (update c) (readFile file) >>= \case
            Left err -> prError $ show err
            Right c' -> return $ Single c'
        Nothing   -> argsError "\"-r\": No configuration file"
      "--read-config"            -> case next of
        Nothing -> argsError "\"--read-config\": No configuration file"
        _       -> parseArgument c "-r" next
      "-w"                       -> case next of
        Just file ->
          return $ Single $ c { saveConfig = file : saveConfig c }
        Nothing   -> argsError "\"-w\": Missing file path"
      "--write-config"           -> case next of
        Nothing -> argsError "\"--write-config\": Missing file path"
        _       -> parseArgument c "-w" next
      "-f"                       -> case next of
        Just x  -> case safeConvert x of
          Left err -> prError $ show err
          Right y -> return $ Single $ c { outputFormat = y }
        Nothing ->
          argsError "\"-f\": No format given"
      "--format"                 -> case next of
        Nothing -> argsError "\"--format\": No format given"
        _       -> parseArgument c "-f" next
      "-m"                       -> case next of
        Just "pretty" -> return $ Single $ c { outputMode = Pretty }
        Just "fully"  -> return $ Single $ c { outputMode = Fully }
        Just x        -> argsError ("Unknown mode: " ++ x)
        Nothing       -> argsError "\"-m\": No mode given"
      "-q"                       -> case next of
        Just "none" -> return $ Single $ c { quoteMode = NoQuotes }
        Just "double"  -> return $ Single $ c { quoteMode = DoubleQuotes }
        Just x        -> argsError ("Unknown quote mode: " ++ x)
        Nothing       -> argsError "\"-q\": No quote mode given"
      "--mode"                   -> case next of
        Nothing -> argsError "\"--mode\": no mode given"
        _       -> parseArgument c "-m" next
      "--quote"                   -> case next of
        Nothing -> argsError "\"--quote\": no quote mode given"
        _       -> parseArgument c "-q" next
      "-pf"                      -> case next of
        Just x  -> return $ Single $ c { partFile = Just x }
        Nothing -> argsError "\"-pf\": No partition file"
      "-bd"                      -> case next of
        Just x  -> return $ Single $ c { busDelimiter = x }
        Nothing -> argsError "\"-bd\": No delimiter given"
      "--bus-delimiter"          -> case next of
        Nothing -> argsError "\"--bus-delimiter\": No delimiter given"
        _       -> parseArgument c "-bd" next
      "-ps"                      -> case next of
        Just x  -> return $ Single $ c { primeSymbol = x }
        Nothing -> argsError "\"-ps\": No symbol replacement given"
      "--prime-symbol"           -> case next of
        Just x  -> return $ Single $ c { primeSymbol = x }
        Nothing -> argsError "\"--prime-symbol\": No symbol replacement given"
      "-as"                      -> case next of
        Just x  -> return $ Single $ c { atSymbol = x }
        Nothing -> argsError "\"-as\": No symbol replacement given"
      "--at-symbol"              -> case next of
        Just x  -> return $ Single $ c { atSymbol = x }
        Nothing -> argsError "\"--at-symbol\": No symbol replacement given"
      "-in"                      -> return $ None $ c { fromStdin = True }
      "-os"                      -> case next of
        Just x  -> case safeConvert x of
          Left err -> prError $ show err
          Right y  -> return $ Single $ c { owSemantics = Just y }
        Nothing -> argsError "\"-os\": No semantics given"
      "--overwrite-semantics"    -> case next of
        Nothing -> argsError "\"--overwrite-semantics\": No semantics given"
        _       -> parseArgument c "-os" next
      "-ot"                      -> case next of
        Just x  -> case safeConvert x of
          Left err -> prError $ show err
          Right y  -> return $ Single $ c { owTarget = Just y }
        Nothing -> argsError "\"-ot\": No target given"
      "--overwrite-target"       -> case next of
        Nothing -> argsError "\"--overwrite-target\": No target given"
        _       -> parseArgument c "-ot" next
      "-op"                      -> case next of
        Just x  -> case parse parameterParser "Overwrite Parameter Error" x of
          Left err -> prError $ show err
          Right y  -> return $ Single $ c { owParameter = y : owParameter c }
        Nothing -> argsError "\"-op\": No parameter given"
      "--overwrite-parameter"    -> case next of
        Nothing -> argsError "\"--overwrite-parameter\": No parameter given"
        _       -> parseArgument c "-op" next
      "-s0"                      -> simple $ c { simplifyWeak = True }
      "-s1"                      -> simple $ c { simplifyStrong = True }
      "-nnf"                     -> simple $ c { negNormalForm = True }
      "-pgi"                     -> simple $ c { pushGlobally = True }
      "-pfi"                     -> simple $ c { pushFinally = True }
      "-pxi"                     -> simple $ c { pushNext = True }
      "-pgo"                     -> simple $ c { pullGlobally = True }
      "-pfo"                     -> simple $ c { pullFinally = True }
      "-pxo"                     -> simple $ c { pullNext = True }
      "-nw"                      -> simple $ c { noWeak = True }
      "-nr"                      -> simple $ c { noRelease = True }
      "-nf"                      -> simple $ c { noFinally = True }
      "-ng"                      -> simple $ c { noGlobally = True }
      "-nd"                      -> simple $ c { noDerived = True }
      "-gr"                      -> simple $ (clean c) { cGR = True }
      "-c"                       -> simple $ (clean c) { check = True }
      "-t"                       -> simple $ (clean c) { pTitle = True }
      "-d"                       -> simple $ (clean c) { pDesc = True }
      "-s"                       -> simple $ (clean c) { pSemantics = True }
      "-g"                       -> simple $ (clean c) { pTarget = True }
      "-a"                       -> simple $ (clean c) { pTags = True }
      "-p"                       -> simple $ (clean c) { pParameters = True }
      "-ins"                     -> simple $ (clean c) { pInputs = True }
      "-outs"                    -> simple $ (clean c) { pOutputs = True }
      "-i"                       -> simple $ (clean c) { pInfo = True }
      "-v"                       -> simple $ (clean c) { pVersion = True }
      "-h"                       -> simple $ (clean c) { pHelp = True }
      "--readme"                 -> simple $ (clean c) { pReadme = True }
      "--readme.md"              -> simple $ (clean c) { pReadmeMd = True }
      "--part-file"              -> parseArgument c "-pf" next
      "--stdin"                  -> parseArgument c "-in" next
      "--weak-simplify"          -> parseArgument c "-s0" next
      "--strong-simplify"        -> parseArgument c "-s1" next
      "--negation-normal-form"   -> parseArgument c "-nnf" next
      "--push-globally-inwards"  -> parseArgument c "-pgi" next
      "--push-finally-inwards"   -> parseArgument c "-pfi" next
      "--push-next-inwards"      -> parseArgument c "-pni" next
      "--pull-globally-outwards" -> parseArgument c "-pgo" next
      "--pull-finally-outwards"  -> parseArgument c "-pfo" next
      "--pull-next-outwards"     -> parseArgument c "-pxo" next
      "--no-weak-until"          -> parseArgument c "-nw" next
      "--no-realease"            -> parseArgument c "-nr" next
      "--no-finally"             -> parseArgument c "-nf" next
      "--no-globally"            -> parseArgument c "-ng" next
      "--no-derived"             -> parseArgument c "-nd" next
      "--generalized-reactivity" -> parseArgument c "-gr" next
      "--check"                  -> parseArgument c "-c" next
      "--print-title"            -> parseArgument c "-t" next
      "--print-description"      -> parseArgument c "-d" next
      "--print-semantics"        -> parseArgument c "-s" next
      "--print-target"           -> parseArgument c "-g" next
      "--print-tags"             -> parseArgument c "-a" next
      "--print-parameters"       -> parseArgument c "-p" next
      "--print-input-signals"    -> parseArgument c "-ins" next
      "--print-output-signals"   -> parseArgument c "-outs" next
      "--print-info"             -> parseArgument c "-i" next
      "--version"                -> parseArgument c "-v" next
      "--help"                   -> parseArgument c "-h" next
      _                          -> return $ None $ c {
                                     inputFiles = arg : inputFiles c
                                     }

    argsError str = do
      prError $ "\"Error\" " ++ str

    clean a = a {
      check = False,
      pTitle = False,
      pDesc = False,
      pSemantics = False,
      pTarget = False,
      pParameters = False,
      pInfo = False,
      pVersion = False,
      pHelp = False,
      pReadme = False,
      pReadmeMd = False
      }

    simple = return . None

-----------------------------------------------------------------------------

parameterParser
  :: Parser (String, Int)

parameterParser = do
  name <-
    identifier $ makeTokenParser
      emptyDef
      { identStart     = letter <|> char '_' <|> char '@'
      , identLetter    = alphaNum <|> char '_' <|> char '@' <|> char '\''
      }

  void $ char '='
  x <- many1 digit
  eof
  return (name, read x)

-----------------------------------------------------------------------------
