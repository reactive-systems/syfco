-----------------------------------------------------------------------------
-- |
-- Module      :  Config
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- Configuration of the tool, set up via the command line arguments.
--
-----------------------------------------------------------------------------

{-# LANGUAGE

    LambdaCase

  #-}

-----------------------------------------------------------------------------

module Config
  ( Configuration(..)
  , defaultCfg
  , readCfg
  , writeCfg
  , checkCfg
  ) where

-----------------------------------------------------------------------------

import Data.Char
  ( toLower
  )

import Data.Info
  ( name
  , version
  , defaultDelimiter
  , defaultPrimeSymbol
  , defaultAtSymbol
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
  , parseError
  , cfgError
  )

import Writer.Formats
  ( WriteFormat(..)
  )

import Text.Parsec.String
  ( Parser
  )

import Print
  ( Print(..)
  )

import Parse
  ( Parse(..)
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
  , unless
  )

import Writer.Data
  ( WriteMode(..)
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

-- | The data type contains all flags and settings
-- that can be adjusted to influence the behavior of the library:

data Configuration =
  Configuration
  { inputFiles :: [FilePath]
    -- ^ The list of input files containing the specifications.

  , outputFile :: Maybe FilePath
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
    -- ^ The delimiter string to seperate the bus index from the
    -- signal name.
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
-- inputFiles     = []
-- outputFile     = Nothing
-- outputFormat   = FULL
-- outputMode     = Pretty
-- partFile       = Nothing
-- busDelimiter   = "_"
-- primeSymbol    = "'"
-- atSymbol       = "@"
-- fromStdin      = False
-- owSemantics    = Nothing
-- owTarget       = Nothing
-- owParameter    = []
-- simplifyWeak   = False
-- simplifyStrong = False
-- negNormalForm  = False
-- pushGlobally   = False
-- pushFinally    = False
-- pushNext       = False
-- pullGlobally   = False
-- pullFinally    = False
-- pullNext       = False
-- noWeak         = False
-- noRelease      = False
-- noFinally      = False
-- noGlobally     = False
-- noDerived      = False
-- cGR            = False
-- check          = False
-- pTitle         = False
-- pDesc          = False
-- pSemantics     = False
-- pTarget        = False
-- pTags          = False
-- pParameters    = False
-- pInputs        = False
-- pOutputs       = False
-- pInfo          = False
-- pVersion       = False
-- pHelp          = False
-- pReadme        = False
-- pReadmeMd      = False
-- saveConfig     = []
-- @

defaultCfg
  :: Configuration

defaultCfg = Configuration
  { inputFiles     = []
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

-- | Checks a configuration for a unresolvable parameter combinations.

checkCfg
  :: Configuration -> Either Error ()

checkCfg cfg
  | pHelp cfg || pVersion cfg || pReadme cfg || pReadmeMd cfg =

      return ()

  | null (inputFiles cfg) && not(fromStdin cfg) && null (saveConfig cfg) =

      cfgError
        "No input specified."

  | not (null (inputFiles cfg)) && fromStdin cfg =

      cfgError
        "Select either \"-in, --stdin\" or give an input file."

  | pushGlobally cfg && pullGlobally cfg =

      cfgError $
        "Select either \"-pgi, --push-globally-inwards\" or " ++
        "\"-pgo, --pull-globally-outwards\"."

  | pushFinally cfg && pullFinally cfg =

      cfgError $
        "Select either \"-pfi, --push-finally-inwards\" or " ++
        "\"-pfo, --pull-finally-outwards\"."

  | pushNext cfg && pullNext cfg =

      cfgError $
        "Select either \"-pxi, --push-next-inwards\" or " ++
        "\"-pxo, --pull-next-outwards\"."

  | simplifyStrong cfg && (pushGlobally cfg || pushFinally cfg ||
                           pushNext cfg || noFinally cfg ||
                           noGlobally cfg || noDerived cfg) =

      cfgError $
        "The flag 'Advanced Simplifications' cannot be combined " ++
        "with any other non-included transformation."

  | negNormalForm cfg && noRelease cfg && noGlobally cfg && noWeak cfg =

      cfgError $
        "The given combination of transformations " ++
        "(negation normal form, no release operators, " ++
        "no globally operators, and no weak until operatators)" ++
        "is impossible to satisfy.\n" ++
        "Remove at least one of these constraints."

  | negNormalForm cfg && noRelease cfg && noDerived cfg =

      cfgError $
        "The given combination of transformations " ++
        "(negation normal form, no release operatators, " ++
        "and no derived operators) is impossible to satisfy.\n" ++
        "Remove at least one of these constraints."

  | negNormalForm cfg && noRelease cfg &&
    (noGlobally cfg || noDerived cfg) && outputFormat cfg == LTLXBA =

      cfgError $
        "The given combination of transformations " ++
        "(negation normal form, no release operators, and " ++
        "no globally / derived operators) " ++
        "is impossible to satisfy when outputting to the " ++
        "LTL2BA / LTL3BA format, since it does not support " ++
        "the weak until operator.\n" ++
        "Remove at least one of these constraints."

  | negNormalForm cfg && noRelease cfg &&
    (noGlobally cfg || noDerived cfg) && outputFormat cfg == WRING =

      cfgError $
        "The given combination of transformations " ++
        "(negation normal form, no release operators, and " ++
        "no globally / derived operators) " ++
        "is impossible to satisfy when outputting to the " ++
        "Wring format, since it does not support " ++
        "the weak until operator.\n" ++
        "Remove at least one of these constraints."

  | negNormalForm cfg && noRelease cfg &&
    (noGlobally cfg || noDerived cfg) && outputFormat cfg == LILY =

      cfgError $
        "The given combination of transformations " ++
        "(negation normal form, no release operators, and " ++
        "no globally / derived operators) " ++
        "is impossible to satisfy when outputting to the " ++
        "Lily format, since it does not support " ++
        "the weak until operator.\n" ++
        "Remove at least one of these constraints."

  | negNormalForm cfg &&
    (noGlobally cfg || noDerived cfg) && outputFormat cfg == ACACIA =

      cfgError $
        "The given combination of transformations " ++
        "(negation normal form, no release operators, and " ++
        "no globally / derived operators) " ++
        "is impossible to satisfy when outputting to the " ++
        "Acacia/Aciacia+ format, since it does not support " ++
        "the weak until nor the release operator.\n" ++
        "Remove at least one of these constraints."

  | negNormalForm cfg && noRelease cfg &&
    (noGlobally cfg || noDerived cfg) && outputFormat cfg == SMV =

      cfgError $
        "The given combination of transformations " ++
        "(negation normal form, no release operators, and " ++
        "no globally / derived operators) " ++
        "is impossible to satisfy when outputting to the " ++
        "SMV format, since it does not support " ++
        "the weak until operator.\n" ++
        "Remove at least one of these constraints."

  | negNormalForm cfg && noGlobally cfg && outputFormat cfg == PSL =

      cfgError $
        "The given combination of transformations " ++
        "(negation normal form and no globally operators)" ++
        "is impossible to satisfy when outputting to the " ++
        "PSL format, since it does not support " ++
        "the weak until and the release operator.\n" ++
        "Remove at least one of these constraints."

  | negNormalForm cfg && noDerived cfg && outputFormat cfg == PSL =

      cfgError $
        "The given combination of transformations " ++
        "(negation normal form and no derived operators)" ++
        "is impossible to satisfy when outputting to the " ++
        "PSL format, since it does not support " ++
        "the release operator.\n" ++
        "Remove at least one of these constraints."

  | negNormalForm cfg && noDerived cfg && outputFormat cfg == UNBEAST =

      cfgError $
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

      cfgError $
        "Applying adaptions is only possible, when transforming to " ++
        "low level backends.\n Returning full TLSF only " ++
        "allows to change parameters."

  | otherwise = return ()

  where
    missingQuotes str =
      length str < 2 ||
      head str /= '"' ||
      last str /= '"'

-----------------------------------------------------------------------------

-- | Creates a configuration file from the given configuration.

writeCfg
  :: Configuration -> String

writeCfg c = unlines
  [ comment "This configuration file has been automatically " ++
    "generated using"
  , comment $ name ++ " (v" ++ version ++
    "). To reload the configuration pass this file to "
  , comment $ name ++ " via '-c <path to config file>'. " ++
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
  , comment $ "\"" ++ name ++ " --help\" to check for possible " ++
    "values."
  , set "format" $ toString $ outputFormat c
  , emptyline
  , comment $ "Specifies the representation mode of the output. " ++
    "Use "
  , comment $ "\"" ++ name ++ " --help\" to check for possible " ++
    "values."
  , set "mode" $ toString $ outputMode c
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
  , ifJust (owSemantics c) "overwrite_semantics" toString
  , emptyline
  , comment $ "Overwrites the target of the input " ++
    "specification. Do not set"
  , comment "to keep the value unchanged."
  , ifJust (owTarget c) "overwrite_target" toString
  , emptyline
  , comment $ "Either enable or disable weak simplifications on " ++
    "the LTL"
  , comment $ "formula level. Possible values are either \"true\" " ++
    "or \"false\"."
  , set "weak_simplify" $ toString $ simplifyWeak c
  , emptyline
  , comment $ "Either enable or disable strong simplifications on " ++
    "the LTL"
  , comment $ "formula level. Possible values are either \"true\" " ++
    "or \"false\"."
  , set "strong_simplify" $ toString $ simplifyStrong c
  , emptyline
  , comment $ "Either enable or disable that the resulting " ++
    "formula is"
  , comment "converted into negation normal form. Possible values " ++
    "are"
  , comment "either \"true\" or \"false\"."
  , set "negation_normal_form" $ toString $ negNormalForm c
  , emptyline
  , comment $ "Either enable or disable to push globally operators " ++
    "inwards,"
  , comment "i.e., to apply the following equivalence:"
  , comment ""
  , comment "  G (a && b) => (G a) && (G b)"
  , comment ""
  , comment "Possible values are either \"true\" or \"false\"."
  , set "push_globally_inwards" $ toString $ pushGlobally c
  , emptyline
  , comment $ "Either enable or disable to push finally operators " ++
    "inwards,"
  , comment "i.e., to apply the following equivalence:"
  , comment ""
  , comment "  F (a || b) => (F a) || (F b)"
  , comment ""
  , comment "Possible values are either \"true\" or \"false\"."
  , set "push_finally_inwards" $ toString $ pushFinally c
  , emptyline
  , comment $ "Either enable or disable to next operators " ++
    "inwards, i.e.,"
  , comment "to apply the following equivalences:"
  , comment ""
  , comment "  X (a && b) => (X a) && (X b)"
  , comment "  X (a || b) => (X a) || (X b)"
  , comment ""
  , comment "Possible values are either \"true\" or \"false\"."
  , set "push_next_inwards" $ toString $ pushNext c
  , emptyline
  , comment $ "Either enable or disable to pull globally operators " ++
    "outwards,"
  , comment "i.e., to apply the following equivalence:"
  , comment ""
  , comment "  (G a) && (G b) => G (a && b)"
  , comment ""
  , comment "Possible values are either \"true\" or \"false\"."
  , set "pull_globally_outwards" $ toString $ pullGlobally c
  , emptyline
  , comment $ "Either enable or disable to pull finally operators " ++
    "outwards,"
  , comment "i.e., to apply the following equivalence:"
  , comment ""
  , comment "  (F a) || (F b) => F (a || b)"
  , comment ""
  , comment "Possible values are either \"true\" or \"false\"."
  , set "pull_finally_outwards" $ toString $ pullFinally c
  , emptyline
  , comment $ "Either enable or disable to pull next operators " ++
    "outwards,"
  , comment "i.e., to apply the following equivalences:"
  , comment ""
  , comment "  (X a) && (X b) => X (a && b)"
  , comment "  (X a) || (X b) => X (a || b)"
  , comment ""
  , comment "Possible values are either \"true\" or \"false\"."
  , set "pull_next_outwards" $ toString $ pullNext c
  , emptyline
  , comment $ "Either enable or disable to resolve weak until " ++
    "operators."
  , comment "Possible values are either \"true\" or \"false\"."
  , set "no_weak_until" $ toString $ noWeak c
  , emptyline
  , comment $ "Either enable or disable to resolve release " ++
    "operators."
  , comment "Possible values are either \"true\" or \"false\"."
  , set "no_release" $ toString $ noRelease c
  , emptyline
  , comment $ "Either enable or disable to resolve finally " ++
    "operators."
  , comment "Possible values are either \"true\" or \"false\"."
  , set "no_finally" $ toString $ noFinally c
  , emptyline
  , comment $ "Either enable or disable to resolve globally " ++
    "operators."
  , comment "Possible values are either \"true\" or \"false\"."
  , set "no_globally" $ toString $ noGlobally c
  , emptyline
  , comment $ "Either enable or disable to resolve derived " ++
    "operators, i.e.,"
  , comment "weak until, finally, globally, ... . Possible " ++
    "values are"
  , comment "either \"true\" or \"false\"."
  , set "no_derived" $ toString $ noDerived c
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

-- | Parses a configuration from the content of a configuration file.

readCfg
  :: Configuration -> String -> Either Error Configuration

readCfg c str =
  case P.parse configParser "Configuration Error" str of
    Left err -> parseError err
    Right xs -> return $ foldl (\x f -> f x) c xs

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
      case fromString v of
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
