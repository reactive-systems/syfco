-----------------------------------------------------------------------------
-- |
-- Module      :  Config
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- Configuration of the tool, set up via the command line arguments.
--
-----------------------------------------------------------------------------

{-# LANGUAGE LambdaCase #-}

-----------------------------------------------------------------------------

module Config
  ( Configuration(..)
  , defaultCfg
  , parseArguments
  , printableConfig
  ) where

-----------------------------------------------------------------------------

import Data.Char
  ( toLower
  )

import Data.Info
  ( toolName
  , toolVersion
  , defaultDelimiter
  , defaultPrimeSymbol
  , defaultAtSymbol
  )

import Data.Utils
  ( MachinePrintable(..)
  )

import Data.Maybe
  ( isJust
  , catMaybes
  )

import System.Directory
  ( doesFileExist
  )

import Data.Types
  ( Semantics(..)
  , Target(..)
  )

import Data.Error
  ( Error
  , prError
  , argsError
  , parseError
  )

import Writer.Formats
  ( WriteFormat(..)
  )

import Text.Parsec.String
  ( Parser
  )

import Text.Parsec
  ( (<|>)
  , (<?>)
  , char
  , oneOf
  , many
  , many1
  , digit
  , alphaNum
  , eof
  )

import Control.Monad
  ( void
  , liftM
  , when
  )

import Writer.Data
  ( WriteMode(..)
  )

import Generics.Deriving.Enum
  ( GEnum
  )

import Reader.Parser.Info
  ( targetParser
  , semanticsParser
  )

import Reader.Parser.Data
  ( globalDef
  )

import Text.Parsec.Prim
  ( parserZero
  )

import Text.Parsec.Token
  ( LanguageDef
  , GenLanguageDef(..)
  , makeTokenParser
  , stringLiteral
  , identifier
  , reserved
  , reservedOp
  , whiteSpace
  )

import Text.Parsec.Language
  ( emptyDef
  )

import qualified Text.Parsec as P

-----------------------------------------------------------------------------

-- | The @Configuartion@ data type contains all flags and settings
-- that can be adjusted via the command line arguments. This includes:

data Configuration =
  Configuration
  { inputFile :: [String]
    -- ^ The list of input files containing the specifications.

  , outputFile :: Maybe String
    -- ^ An optional path to the output file, the transformed
    -- specification is written to.

  , outputFormat :: WriteFormat
    -- ^ The format specifiying the corresponding writer to use.
    --
    --   /(can be changed via a configuration file, use:/ &#160;
    --   @ format = ... @ /)/

  , outputMode :: WriteMode
    -- ^ The output mode used by the writer.
    --
    --   /(can be changed via a configuration file, use:/ &#160;
    --   @ mode = ... @ /)/

  , partFile :: Maybe String
    -- ^ Optional path to a parition file, which is created if
    -- set.

  , busDelimiter :: String
    -- ^ The delimiter string to seperate the bus index from the b
    -- signa name.
    --
    --   /(can be changed via a configuration file, use:/ &#160;
    --   @ bus_delimiter = ... @ /)/

  , primeSymbol :: String
    -- ^ The prime symbol \/ string representing primes in signals of
    -- the input format.
    --
    --   /(can be changed via a configuration file, use:/ &#160;
    --   @ prime_symbol = ... @ /)/

  , atSymbol :: String
    -- ^ The at symbol \/ string representing at symbols in signals
    -- of the input format.
    --
    --   /(can be changed via a configuration file, use:/ &#160;
    --   @ at_symbol = ... @ /)/

  , fromStdin :: Bool
    -- ^ A boolean flag specifying whether the input should be read
    -- from STDIN or not.

  , owSemantics :: Maybe Semantics
    -- ^ An optional flag which allows to overwrite the semantics of
    -- the given input specifications.
    --
    --   /(can be changed via a configuration file, use:/ &#160;
    --   @ overwrite_semantics = ... @ /)/

  , owTarget :: Maybe Target
    -- ^ An optional flag which allows to overwrite the target of
    -- the given input specifications.
    --
    --   /(can be changed via a configuration file, use:/ &#160;
    --   @ overwrite_target = ... @ /)/

  , owParameter :: [(String,Int)]
    -- ^ An optional flag which allows to overwrite a list of
    -- parameters of the given input specification.

  , simplifyWeak :: Bool
    -- ^ A boolean flag specifying whether weak simplifications
    -- should be applied or not.
    --
    --   /(can be changed via a configuration file, use:/ &#160;
    --   @ weak_simplify = ... @ /)/

  , simplifyStrong :: Bool
    -- ^ A boolean flag specifying whether strong simplifications
    -- should be applied or not.
    --
    --   /(can be changed via a configuration file, use:/ &#160;
    --   @ strong_simplify = ... @ /)/

  , negNormalForm :: Bool
    -- ^ A boolean flag specifying whether the given specification
    -- should be turned into negation normal form.
    --
    --   /(can be changed via a configuration file, use:/ &#160;
    --   @ negation_normal_form = ... @ /)/

  , pushGlobally :: Bool
    -- ^ A boolean flag specifying whether globally operators should
    -- be pushed over conjunctions deeper into the formula.
    --
    --   /(can be changed via a configuration file, use:/ &#160;
    --   @ push_globally_inwards = ... @ /)/

  , pushFinally :: Bool
    -- ^ A boolean flag specifying whether finally operators should
    -- be pushed over disjunctions deeper into the formula.
    --
    --   /(can be changed via a configuration file, use:/ &#160;
    --   @ push_finally_inwards = ... @ /)/

  , pushNext :: Bool
    -- ^ A boolean flag specifying whether next operators should be
    -- pushed over conjunctions and disjunctions deeper into the
    -- formula.
    --
    --   /(can be changed via a configuration file, use:/ &#160;
    --   @ push_next_inwards = ... @ /)/

  , pullGlobally :: Bool
    -- ^ A boolean flag specifying whether globally perators should
    -- be pulled over conjunctions outside the formula.
    --
    --   /(can be changed via a configuration file, use:/ &#160;
    --   @ pull_globally_outwards = ... @ /)/

  , pullFinally :: Bool
    -- ^ A boolean flag specifying whether finally operators should
    -- be pulled over disjunctions outside the formula.
    --
    --   /(can be changed via a configuration file, use:/ &#160;
    --   @ pull_finally_outwards = ... @ /)/

  , pullNext :: Bool
    -- ^ A boolean flag specifying whether next operators should be
    -- pulled over conjunctions and disjunctions outside the
    -- formula.
    --
    --   /(can be changed via a configuration file, use:/ &#160;
    --   @ pull_next_outwards = ... @ /)/

  , noWeak :: Bool
    -- ^ A boolean flag specifying whether weak until operators
    -- should be replaced by alternative operators inside the
    -- created formula.
    --
    --   /(can be changed via a configuration file, use:/ &#160;
    --   @ no_weak_until = ... @ /)/

  , noRelease :: Bool
    -- ^ A boolean flag specifying whether release operators should
    -- be replaced by alternative operators inside the created
    -- formula.
    --
    --   /(can be changed via a configuration file, use:/ &#160;
    --   @ no_release = ... @ /)/

  , noFinally :: Bool
    -- ^ A boolean flag specifying whether finally operators should
    -- be replaced by alternative operators inside the created
    -- formula.
    --
    --   /(can be changed via a configuration file, use:/ &#160;
    --   @ no_finally = ... @ /)/

  , noGlobally :: Bool
    -- ^ A boolean flag specifying whether globally operators should
    -- be replaced by alternative operators inside the created
    -- formula.
    --
    --   /(can be changed via a configuration file, use:/ &#160;
    --   @ no_globally = ... @ /)/

  , noDerived :: Bool
    -- ^ A boolean flag specifying whether any derived operators
    -- should be replaced by alternative operators inside the
    -- created formula.
    --
    --   /(can be changed via a configuration file, use:/ &#160;
    --   @ no_derived = ... @ /)/

  , cGR :: Bool
    -- ^ A boolean flag specifying whether to check, whether the
    -- input belongs to the class of Generalized Reactivity
    -- specifications or not.

  , check :: Bool
    -- ^ A boolean flag specifying whether the given input files
    -- should just be checked for syntactical and type correctenss.

  , pTitle :: Bool
    -- ^ A boolean flag specifying whether just the title of the
    -- given input files should be printed or not.

  , pDesc :: Bool
    -- ^ A boolean flag specifying whether just the description of
    -- the given input files should be printed or not.

  , pSemantics :: Bool
    -- ^ A boolean flag specifying whether just the semantics of
    -- the given input files should be printed or not.

  , pTarget :: Bool
    -- ^ A boolean flag specifying whether just the target of the
    -- given input files should be printed or not.

  , pTags :: Bool
    -- ^ A boolean flag specifying whether just the tag list of
    -- the given input files should be printed or not,

  , pParameters :: Bool
    -- ^ A boolean flag specifying whether just the parameter list
    -- of the given specification should be printed or not.

  , pInputs :: Bool
    -- ^ A boolean flag specifying whether just the input signals
    -- of the given specification should be printed or not.

  , pOutputs :: Bool
    -- ^ A boolean flag specifying whether just the output signals
    -- of the given specification should be printed or not.

  , pInfo :: Bool
    -- ^ A boolean flag specifying whether just the complete input
    -- section of the given input files should be printed or not.

  , pVersion :: Bool
    -- ^ A boolean flag specifying whether the version info should
    -- be printed or not.

  , pHelp :: Bool
    -- ^ A boolean flag specifying whether the help info should be
    -- printed or not.
  , pReadme :: Bool
    -- ^ A boolean flag specifying whether the content of the README
    -- file should be printed to STDOUT or not.
  , pReadmeMd :: Bool
    -- ^ A boolean flag specifying whether the content of the
    -- README.md file should be printed to STDOUT or not.
  , saveConfig :: [FilePath]
    -- ^ List of file paths to store the current configuration.
  }

-----------------------------------------------------------------------------

-- | The default configuration.
--
-- @
-- inputFile = []
-- outputFile = Nothing
-- outputFormat = FULL
-- outputMode = Pretty
-- partFile = Nothing
-- busDelimiter = "_"
-- primeSymbol = "'"
-- atSymbol = "@"
-- fromStdin = False
-- owSemantics = Nothing
-- owTarget = Nothing
-- owParameter = []
-- simplifyWeak = False
-- simplifyStrong = False
-- negNormalForm = False
-- pushGlobally = False
-- pushFinally = False
-- pushNext = False
-- pullGlobally = False
-- pullFinally = False
-- pullNext = False
-- noWeak = False
-- noRelease = False
-- noFinally = False
-- noGlobally = False
-- noDerived = False
-- cGR = False
-- check = False
-- pTitle = False
-- pDesc = False
-- pSemantics = False
-- pTarget = False
-- pTags = False
-- pParameters = False
-- pInputs = False
-- pOutputs = False
-- pInfo = False
-- pVersion = False
-- pHelp = False
-- pReadme = False
-- pReadmeMd = False
-- saveConfig = []
-- @

defaultCfg
  :: Configuration

defaultCfg = Configuration
  { inputFile      = []
  , outputFile     = Nothing
  , outputFormat   = FULL
  , outputMode     = Pretty
  , partFile       = Nothing
  , busDelimiter   = defaultDelimiter
  , primeSymbol    = defaultPrimeSymbol
  , atSymbol       = defaultAtSymbol
  , fromStdin      = False
  , owSemantics    = Nothing
  , owTarget       = Nothing
  , owParameter    = []
  , simplifyWeak   = False
  , simplifyStrong = False
  , negNormalForm  = False
  , pushGlobally   = False
  , pushFinally    = False
  , pushNext       = False
  , pullGlobally   = False
  , pullFinally    = False
  , pullNext       = False
  , noWeak         = False
  , noRelease      = False
  , noFinally      = False
  , noGlobally     = False
  , noDerived      = False
  , cGR            = False
  , check          = False
  , pTitle         = False
  , pDesc          = False
  , pSemantics     = False
  , pTarget        = False
  , pTags          = False
  , pParameters    = False
  , pInputs        = False
  , pOutputs       = False
  , pInfo          = False
  , pVersion       = False
  , pHelp          = False
  , pReadme        = False
  , pReadmeMd      = False
  , saveConfig     = []
  }

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
  checkConfiguration c
  return c

  where
    traverse c xs = case xs of
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
        Nothing -> aErr "\"-o\": No output file"
      "--output"                 -> case next of
        Nothing -> aErr "\"--output\": No output file"
        _       -> parseArgument c "-o" next
      "-r"                       -> case next of
        Just file -> do
          exists <- doesFileExist file
          when (not exists) $ aErr $ "File does not exist: " ++ file
          readFile file >>= return . parseConfig c >>= \case
            Left err -> error "todo"
            Right c' -> return $ Single c'
        Nothing   -> aErr "\"-r\": No configuration file"
      "--read-config"            -> case next of
        Nothing -> aErr "\"--read-config\": No configuration file"
        _       -> parseArgument c "-r" next
      "-w"                       -> case next of
        Just file -> do
          return $ Single $ c { saveConfig = file : saveConfig c }
        Nothing   -> aErr "\"-w\": Missing file path"
      "--write-config"           -> case next of
        Nothing -> aErr "\"--write-config\": Missing file path"
        _       -> parseArgument c "-w" next
      "-f"                       -> case next of
        Just x  -> case mparse x of
          Left err -> prError err
          Right y -> return $ Single $ c { outputFormat = y }
        Nothing ->
          aErr "\"-f\": No format given"
      "--format"                 -> case next of
        Nothing -> aErr "\"--format\": No format given"
        _       -> parseArgument c "-f" next
      "-m"                       -> case next of
        Just "pretty" -> return $ Single $ c { outputMode = Pretty }
        Just "fully"  -> return $ Single $ c { outputMode = Fully }
        Just x        -> aErr ("Unknown mode: " ++ x)
        Nothing       -> aErr "\"-m\": No mode given"
      "--mode"                   -> case next of
        Nothing -> aErr "\"--mode\": no mode given"
        _       -> parseArgument c "-m" next
      "-pf"                      -> case next of
        Just x  -> return $ Single $ c { partFile = Just x }
        Nothing -> aErr "\"-pf\": No partition file"
      "-bd"                      -> case next of
        Just x  -> return $ Single $ c { busDelimiter = x }
        Nothing -> aErr "\"-bd\": No delimiter given"
      "--bus-delimiter"          -> case next of
        Nothing -> aErr "\"--bus-delimiter\": No delimiter given"
        _       -> parseArgument c "-bd" next
      "-ps"                      -> case next of
        Just x  -> return $ Single $ c { primeSymbol = x }
        Nothing -> aErr "\"-ps\": No symbol replacement given"
      "--prime-symbol"           -> case next of
        Just x  -> return $ Single $ c { primeSymbol = x }
        Nothing -> aErr "\"--prime-symbol\": No symbol replacement given"
      "-as"                      -> case next of
        Just x  -> return $ Single $ c { atSymbol = x }
        Nothing -> aErr "\"-as\": No symbol replacement given"
      "--at-symbol"              -> case next of
        Just x  -> return $ Single $ c { atSymbol = x }
        Nothing -> aErr "\"--at-symbol\": No symbol replacement given"
      "-in"                      -> return $ None $ c { fromStdin = True }
      "-os"                      -> case next of
        Just x  -> case P.parse semanticsParser "Overwrite Semantics Error" x of
          Left err -> pErr err
          Right (y,_) -> return $ Single $ c { owSemantics = Just y }
        Nothing -> aErr "\"-os\": No semantics given"
      "--overwrite-semantics"    -> case next of
        Nothing -> aErr "\"--overwrite-semantics\": No semantics given"
        _       -> parseArgument c "-os" next
      "-ot"                      -> case next of
        Just x  -> case P.parse targetParser "Overwrite Target Error" x of
          Left err -> pErr err
          Right (y,_) -> return $ Single $ c { owTarget = Just y }
        Nothing -> aErr "\"-ot\": No target given"
      "--overwrite-target"       -> case next of
        Nothing -> aErr "\"--overwrite-target\": No target given"
        _       -> parseArgument c "-ot" next
      "-op"                      -> case next of
        Just x  -> case P.parse parameterParser "Overwrite Parameter Error" x of
          Left err -> pErr err
          Right y  -> return $ Single $ c { owParameter = y : owParameter c }
        Nothing -> aErr "\"-op\": No parameter given"
      "--overwrite-parameter"    -> case next of
        Nothing -> aErr "\"--overwrite-parameter\": No parameter given"
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
                                     inputFile = arg : inputFile c
                                     }

    aErr str =
      let Left err = argsError str
      in prError err

    pErr str =
      let Left err = parseError str
      in prError err

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

checkConfiguration
  :: Configuration -> IO ()

checkConfiguration cfg
  | pHelp cfg || pVersion cfg || pReadme cfg || pReadmeMd cfg =

      return ()

  | null (inputFile cfg) && not(fromStdin cfg) && null (saveConfig cfg) =

      error
        "no input specified"

  | not (null (inputFile cfg)) && fromStdin cfg =

      error
        "Select either \"-in, --stdin\" or give an input file."

  | pushGlobally cfg && pullGlobally cfg =

      error $
        "Select either \"-pgi, --push-globally-inwards\" or " ++
        "\"-pgo, --pull-globally-outwards\"."

  | pushFinally cfg && pullFinally cfg =

      error $
        "Select either \"-pfi, --push-finally-inwards\" or " ++
        "\"-pfo, --pull-finally-outwards\"."

  | pushNext cfg && pullNext cfg =

      error $
        "Select either \"-pxi, --push-next-inwards\" or " ++
        "\"-pxo, --pull-next-outwards\"."

  | simplifyStrong cfg && (pushGlobally cfg || pushFinally cfg ||
                           pushNext cfg || noFinally cfg ||
                           noGlobally cfg || noDerived cfg) =

      error $
        "The flag 'Advanced Simplifications' cannot be combined " ++
        "with any other non-included transformation."

  | negNormalForm cfg && noRelease cfg && noGlobally cfg && noWeak cfg =

      error $
        "The given combination of transformations " ++
        "(negation normal form, no release operators, " ++
        "no globally operators, and no weak until operatators)" ++
        "is impossible to satisfy.\n" ++
        "Remove at least one of these constraints."

  | negNormalForm cfg && noRelease cfg && noDerived cfg =

      error $
        "The given combination of transformations " ++
        "(negation normal form, no release operatators, " ++
        "and no derived operators) is impossible to satisfy.\n" ++
        "Remove at least one of these constraints."

  | negNormalForm cfg && noRelease cfg &&
    (noGlobally cfg || noDerived cfg) && outputFormat cfg == LTLXBA =

      error $
        "The given combination of transformations " ++
        "(negation normal form, no release operators, and " ++
        "no globally / derived operators) " ++
        "is impossible to satisfy when outputting to the " ++
        "LTL2BA / LTL3BA format, since it does not support " ++
        "the weak until operator.\n" ++
        "Remove at least one of these constraints."

  | negNormalForm cfg && noRelease cfg &&
    (noGlobally cfg || noDerived cfg) && outputFormat cfg == WRING =

      error $
        "The given combination of transformations " ++
        "(negation normal form, no release operators, and " ++
        "no globally / derived operators) " ++
        "is impossible to satisfy when outputting to the " ++
        "Wring format, since it does not support " ++
        "the weak until operator.\n" ++
        "Remove at least one of these constraints."

  | negNormalForm cfg && noRelease cfg &&
    (noGlobally cfg || noDerived cfg) && outputFormat cfg == LILY =

      error $
        "The given combination of transformations " ++
        "(negation normal form, no release operators, and " ++
        "no globally / derived operators) " ++
        "is impossible to satisfy when outputting to the " ++
        "Lily format, since it does not support " ++
        "the weak until operator.\n" ++
        "Remove at least one of these constraints."

  | negNormalForm cfg &&
    (noGlobally cfg || noDerived cfg) && outputFormat cfg == ACACIA =

      error $
        "The given combination of transformations " ++
        "(negation normal form, no release operators, and " ++
        "no globally / derived operators) " ++
        "is impossible to satisfy when outputting to the " ++
        "Acacia/Aciacia+ format, since it does not support " ++
        "the weak until nor the release operator.\n" ++
        "Remove at least one of these constraints."

  | negNormalForm cfg && noRelease cfg &&
    (noGlobally cfg || noDerived cfg) && outputFormat cfg == SMV =

      error $
        "The given combination of transformations " ++
        "(negation normal form, no release operators, and " ++
        "no globally / derived operators) " ++
        "is impossible to satisfy when outputting to the " ++
        "SMV format, since it does not support " ++
        "the weak until operator.\n" ++
        "Remove at least one of these constraints."

  | negNormalForm cfg && noGlobally cfg && outputFormat cfg == PSL =

      error $
        "The given combination of transformations " ++
        "(negation normal form and no globally operators)" ++
        "is impossible to satisfy when outputting to the " ++
        "PSL format, since it does not support " ++
        "the weak until and the release operator.\n" ++
        "Remove at least one of these constraints."

  | negNormalForm cfg && noDerived cfg && outputFormat cfg == PSL =

      error $
        "The given combination of transformations " ++
        "(negation normal form and no derived operators)" ++
        "is impossible to satisfy when outputting to the " ++
        "PSL format, since it does not support " ++
        "the release operator.\n" ++
        "Remove at least one of these constraints."

  | negNormalForm cfg && noDerived cfg && outputFormat cfg == UNBEAST =

      error $
        "The given combination of transformations " ++
        "(negation normal form and no derived operators)" ++
        "is impossible to satisfy when outputting to the " ++
        "UNBEAST format, since it does not support " ++
        "the release operator.\n" ++
        "Remove at least one of these constraints."

  | outputFormat cfg == FULL &&
    (isJust (owSemantics cfg) || isJust (owTarget cfg) ||
     simplifyWeak cfg || simplifyStrong cfg || negNormalForm cfg ||
     pushGlobally cfg || pushFinally cfg || pushNext cfg ||
     pullGlobally cfg || pullFinally cfg || pullNext cfg ||
     noWeak cfg || noRelease cfg || noFinally cfg || noGlobally cfg ||
     noDerived cfg) =

      error $
        "Applying adaptions is only possible, when transforming to " ++
        "low level backends.\n Returning full TLSF only " ++
        "allows to change parameters."

  | otherwise = return ()

  where
    error str =
      let Left err = argsError str
      in prError err

    missingQuotes str =
      length str < 2 ||
      head str /= '"' ||
      last str /= '"'

-----------------------------------------------------------------------------

-- | Creates a configuration file out of the current configuration.

printableConfig
  :: Configuration -> String

printableConfig c = unlines
    [ comment "This configuration file has been automatically " ++
      "generated using"
    , comment $ toolName ++ " (v" ++ toolVersion ++
      "). To reload the configuration pass this file to "
    , comment $ toolName ++ " via '-c <path to config file>'. " ++
      "Configuration files can be"
    , comment $ "used together with arguments passed via the command " ++
      "line interface."
    , comment $ "If a parameter occurs multiple times, then it is " ++
      "assigned the last"
    , comment $ "value in the order of declaration. The same principle " ++
      "applies, if"
    , comment "multiple configuration files are loaded."
    , comment ""
    , comment $ "All entries of this configuration file are optional. " ++
      "If not set,"
    , comment $ "either the default values or the values, passed via " ++
      "the command"
    , comment "line arguments, are used."
    , emptyline
    , comment $ "Specifies the format of the generated output file. " ++
      "Use "
    , comment $ "\"" ++ toolName ++ " --help\" to check for possible " ++
      "values."
    , set "format" $ mprint $ outputFormat c
    , emptyline
    , comment $ "Specifies the representation mode of the output. " ++
      "Use "
    , comment $ "\"" ++ toolName ++ " --help\" to check for possible " ++
      "values."
    , set "mode" $ mprint $ outputMode c
    , emptyline
    , comment $ "Specifies the bus delimiter symbol / string. The " ++
      "value has to be "
    , comment "encapsulated into quotation marks."
    , set "bus_delimiter" $ "\"" ++ busDelimiter c ++ "\""
    , emptyline
    , comment $ "Specifies the output representation of prime " ++
      "symbols. The value "
    , comment "has to be encapsulated into quotation marks."
    , set "prime_symbol" $ "\"" ++ primeSymbol c ++ "\""
    , emptyline
    , comment $ "Specifies the output representation of \"@\"-" ++
      "symbols. The value "
    , comment "has to be encapsulated into quotation marks."
    , set "at_symbol" $ "\"" ++ atSymbol c ++ "\""
    , emptyline
    , comment $ "Overwrites the semantics of the input " ++
      "specification. Do not set"
    , comment "to keep the value unchanged."
    , ifJust (owSemantics c) "overwrite_semantics" mprint
    , emptyline
    , comment $ "Overwrites the target of the input " ++
      "specification. Do not set"
    , comment "to keep the value unchanged."
    , ifJust (owTarget c) "overwrite_target" mprint
    , emptyline
    , comment $ "Either enable or disable weak simplifications on " ++
      "the LTL"
    , comment $ "formula level. Possible values are either \"true\" " ++
      "or \"false\"."
    , set "weak_simplify"  $ mprint $ simplifyWeak c
    , emptyline
    , comment $ "Either enable or disable strong simplifications on " ++
      "the LTL"
    , comment $ "formula level. Possible values are either \"true\" " ++
      "or \"false\"."
    , set "strong_simplify" $ mprint $ simplifyStrong c
    , emptyline
    , comment $ "Either enable or disable that the resulting " ++
      "formula is"
    , comment "converted into negation normal form. Possible values " ++
      "are"
    , comment "either \"true\" or \"false\"."
    , set "negation_normal_form" $ mprint $ negNormalForm c
    , emptyline
    , comment $ "Either enable or disable to push globally operators " ++
      "inwards,"
    , comment "i.e., to apply the following equivalence:"
    , comment ""
    , comment "  G (a && b) => (G a) && (G b)"
    , comment ""
    , comment "Possible values are either \"true\" or \"false\"."
    , set "push_globally_inwards" $ mprint $ pushGlobally c
    , emptyline
    , comment $ "Either enable or disable to push finally operators " ++
      "inwards,"
    , comment "i.e., to apply the following equivalence:"
    , comment ""
    , comment "  F (a || b) => (F a) || (F b)"
    , comment ""
    , comment "Possible values are either \"true\" or \"false\"."
    , set "push_finally_inwards" $ mprint $ pushFinally c
    , emptyline
    , comment $ "Either enable or disable to next operators " ++
      "inwards, i.e.,"
    , comment "to apply the following equivalences:"
    , comment ""
    , comment "  X (a && b) => (X a) && (X b)"
    , comment "  X (a || b) => (X a) || (X b)"
    , comment ""
    , comment "Possible values are either \"true\" or \"false\"."
    , set "push_next_inwards" $ mprint $ pushNext c
    , emptyline
    , comment $ "Either enable or disable to pull globally operators " ++
      "outwards,"
    , comment "i.e., to apply the following equivalence:"
    , comment ""
    , comment "  (G a) && (G b) => G (a && b)"
    , comment ""
    , comment "Possible values are either \"true\" or \"false\"."
    , set "pull_globally_outwards" $ mprint $ pullGlobally c
    , emptyline
    , comment $ "Either enable or disable to pull finally operators " ++
      "outwards,"
    , comment "i.e., to apply the following equivalence:"
    , comment ""
    , comment "  (F a) || (F b) => F (a || b)"
    , comment ""
    , comment "Possible values are either \"true\" or \"false\"."
    , set "pull_finally_outwards" $ mprint $ pullFinally c
    , emptyline
    , comment $ "Either enable or disable to pull next operators " ++
      "outwards,"
    , comment "i.e., to apply the following equivalences:"
    , comment ""
    , comment "  (X a) && (X b) => X (a && b)"
    , comment "  (X a) || (X b) => X (a || b)"
    , comment ""
    , comment "Possible values are either \"true\" or \"false\"."
    , set "pull_next_outwards" $ mprint $ pullNext c
    , emptyline
    , comment $ "Either enable or disable to resolve weak until " ++
      "operators."
    , comment "Possible values are either \"true\" or \"false\"."
    , set "no_weak_until" $ mprint $ noWeak c
    , emptyline
    , comment $ "Either enable or disable to resolve release " ++
      "operators."
    , comment "Possible values are either \"true\" or \"false\"."
    , set "no_release" $ mprint $ noRelease c
    , emptyline
    , comment $ "Either enable or disable to resolve finally " ++
      "operators."
    , comment "Possible values are either \"true\" or \"false\"."
    , set "no_finally" $ mprint $ noFinally c
    , emptyline
    , comment $ "Either enable or disable to resolve globally " ++
      "operators."
    , comment "Possible values are either \"true\" or \"false\"."
    , set "no_globally" $ mprint $ noGlobally c
    , emptyline
    , comment $ "Either enable or disable to resolve derived " ++
      "operators, i.e.,"
    , comment "weak until, finally, globally, ... . Possible " ++
      "values are"
    , comment "either \"true\" or \"false\"."
    , set "no_derived" $ mprint $ noDerived c
    , emptyline
    ]

    where
      emptyline = ""
      comment = ("# " ++)
      set s v = s ++ " = " ++ v
      ifJust x s f = case x of
        Nothing -> "#\n# " ++ set s "..."
        Just y  -> set s $ f y

-----------------------------------------------------------------------------

parseConfig
  :: Configuration -> String -> Either Error Configuration

parseConfig c str =
  case P.parse configParser "Configuration Error" str of
    Left err -> parseError err
    Right xs  -> return $ foldl (\x f -> f x) c xs

-----------------------------------------------------------------------------

-- | Configuration file parser, that parses the file to a list of
-- configuration updates to preserve the order inside the
-- configuration file.

configParser
  :: Parser [Configuration -> Configuration]

configParser = (~~) >> many entryParser

  where
    entryParser =
          (mParser "format"
             >>= (\v -> return (\c -> c { outputFormat = v })))
      <|> (mParser "mode"
             >>= (\v -> return (\c -> c { outputMode = v })))
      <|> (sParser "bus_delimiter"
             >>= (\v -> return (\c -> c { busDelimiter = v })))
      <|> (sParser "prime_symbol"
             >>= (\v -> return (\c -> c { primeSymbol = v })))
      <|> (sParser "at_symbol"
             >>= (\v -> return (\c -> c { atSymbol = v })))
      <|> (mParser "overwrite_semantics"
             >>= (\v -> return (\c -> c { owSemantics = Just v })))
      <|> (mParser "overwrite_target"
             >>= (\v -> return (\c -> c { owTarget = Just v })))
      <|> (mParser "weak_simplify"
             >>= (\v -> return (\c -> c { simplifyWeak = v })))
      <|> (mParser "strong_simplify"
             >>= (\v -> return (\c -> c { simplifyStrong = v })))
      <|> (mParser "negation_normal_form"
             >>= (\v -> return (\c -> c { negNormalForm = v })))
      <|> (mParser "push_globally_inwards"
             >>= (\v -> return (\c -> c { pushGlobally = v })))
      <|> (mParser "push_finally_inwards"
             >>= (\v -> return (\c -> c { pushFinally = v })))
      <|> (mParser "push_next_inwards"
             >>= (\v -> return (\c -> c { pushNext = v })))
      <|> (mParser "pull_globally_outwards"
             >>= (\v -> return (\c -> c { pullGlobally = v })))
      <|> (mParser "pull_finally_outwards"
             >>= (\v -> return (\c -> c { pullFinally = v })))
      <|> (mParser "pull_next_outwards"
             >>= (\v -> return (\c -> c { pullNext = v })))
      <|> (mParser "no_weak_until"
             >>= (\v -> return (\c -> c { noWeak = v })))
      <|> (mParser "no_release"
             >>= (\v -> return (\c -> c { noRelease = v })))
      <|> (mParser "no_finally"
             >>= (\v -> return (\c -> c { noFinally = v })))
      <|> (mParser "no_globally"
             >>= (\v -> return (\c -> c { noGlobally = v })))
      <|> (mParser "no_derived"
             >>= (\v -> return (\c -> c { noDerived = v })))

    sParser str = do
      keyword str
      op "="
      stringLiteral tokenparser

    mParser str = do
      keyword str
      op "="
      v <- identifier tokenparser
      case mparse v of
        Left _  -> parserZero <?> v
        Right m -> return m

    tokenparser = makeTokenParser configDef

    keyword = void . reserved tokenparser

    (~~) = whiteSpace tokenparser

    op = reservedOp tokenparser

    configDef = emptyDef
      { opStart = oneOf "="
      , opLetter = oneOf "="
      , identStart = alphaNum
      , identLetter = alphaNum <|> char '_'
      , commentLine = "#"
      , nestedComments = False
      , caseSensitive = True
      , reservedOpNames = ["="]
      , reservedNames =
        [ "format", "mode", "bus_delimiter", "prime_symbol", "at_symbol",
          "overwrite_semantics", "overwrite_target", "weak_simplify",
          "strong_simplify", "negation_normal_form",
          "push_globally_inwards", "push_finally_inwards",
          "push_next_inwards", "pull_globally_outwards",
          "pull_finally_outwards", "pull_next_outwards", "no_weak_until",
           "no_release", "no_finally", "no_globally", "no_derived" ]
  }

-----------------------------------------------------------------------------

parameterParser
  :: Parser (String, Int)

parameterParser = do
  name <- identifier $ makeTokenParser globalDef
  void $ char '='
  x <- many1 digit
  eof
  return (name, read x)

-----------------------------------------------------------------------------
