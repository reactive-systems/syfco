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
  , MultiParamTypeClasses
  , TypeSynonymInstances
  , FlexibleInstances
  , FlexibleContexts
  , RecordWildCards

  #-}

-----------------------------------------------------------------------------

module Config
  ( Configuration(..)
  , defaultCfg
  , update
  , verify
  ) where

-----------------------------------------------------------------------------

import Data.Convertible
  ( Convertible(..)
  , ConvertError(..)
  , safeConvert
  , convert
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
  )

import Data.Types
  ( Semantics(..)
  , Target(..)
  )

import Data.Error
  ( Error
  , parseError
  , cfgError
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
  , alphaNum
  )

import Control.Monad
  ( void
  )

import Writer.Data
  ( WriteMode(..)
  , QuoteMode(..)
  )

import Text.Parsec.Prim
  ( parserZero
  )

import Text.Parsec.Token
  ( GenLanguageDef(..)
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

  , quoteMode :: QuoteMode
    -- ^ The quote mode used by the writer.
    --
    --   /(can be changed via a configuration file, use:/ &#160;
    --   @ quote = ... @ /)/

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
  } deriving (Eq, Ord)

-----------------------------------------------------------------------------

-- |
-- @
-- inputFiles     = []
-- outputFile     = Nothing
-- outputFormat   = FULL
-- outputMode     = Pretty
-- quoteMode      = None
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
  , quoteMode      = NoQuotes
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

-- | Verifies that a configuration does not contain invalid parameter
-- combinations.

verify
  :: Configuration -> Either Error ()

verify Configuration{..}
  | pHelp || pVersion || pReadme || pReadmeMd =

      return ()

  | null inputFiles && not fromStdin && null saveConfig =

      cfgError
        "No input specified."

  | not (null inputFiles) && fromStdin =

      cfgError
        "Select either \"-in, --stdin\" or give an input file."

  | pushGlobally && pullGlobally =

      cfgError $
        "Select either \"-pgi, --push-globally-inwards\" or " ++
        "\"-pgo, --pull-globally-outwards\"."

  | pushFinally && pullFinally =

      cfgError $
        "Select either \"-pfi, --push-finally-inwards\" or " ++
        "\"-pfo, --pull-finally-outwards\"."

  | pushNext && pullNext =

      cfgError $
        "Select either \"-pxi, --push-next-inwards\" or " ++
        "\"-pxo, --pull-next-outwards\"."

  | simplifyStrong && (pushGlobally || pushFinally ||
                      pushNext  || noFinally ||
                      noGlobally || noDerived) =

      cfgError $
        "The flag 'Advanced Simplifications' cannot be combined " ++
        "with any other non-included transformation."

  | negNormalForm && noRelease && noGlobally && noWeak =

      cfgError $
        "The given combination of transformations " ++
        "(negation normal form, no release operators, " ++
        "no globally operators, and no weak until operatators)" ++
        "is impossible to satisfy.\n" ++
        "Remove at least one of these constraints."

  | negNormalForm && noRelease && noDerived =

      cfgError $
        "The given combination of transformations " ++
        "(negation normal form, no release operatators, " ++
        "and no derived operators) is impossible to satisfy.\n" ++
        "Remove at least one of these constraints."

  | negNormalForm && noRelease &&
    (noGlobally || noDerived) && outputFormat == LTLXBA =

      cfgError $
        "The given combination of transformations " ++
        "(negation normal form, no release operators, and " ++
        "no globally / derived operators) " ++
        "is impossible to satisfy when outputting to the " ++
        "LTL2BA / LTL3BA format, since it does not support " ++
        "the weak until operator.\n" ++
        "Remove at least one of these constraints."

  | negNormalForm && noRelease &&
    (noGlobally || noDerived) && outputFormat == WRING =

      cfgError $
        "The given combination of transformations " ++
        "(negation normal form, no release operators, and " ++
        "no globally / derived operators) " ++
        "is impossible to satisfy when outputting to the " ++
        "Wring format, since it does not support " ++
        "the weak until operator.\n" ++
        "Remove at least one of these constraints."

  | negNormalForm && noRelease &&
    (noGlobally || noDerived) && outputFormat == LILY =

      cfgError $
        "The given combination of transformations " ++
        "(negation normal form, no release operators, and " ++
        "no globally / derived operators) " ++
        "is impossible to satisfy when outputting to the " ++
        "Lily format, since it does not support " ++
        "the weak until operator.\n" ++
        "Remove at least one of these constraints."

  | negNormalForm &&
    (noGlobally || noDerived) && outputFormat == ACACIA =

      cfgError $
        "The given combination of transformations " ++
        "(negation normal form, no release operators, and " ++
        "no globally / derived operators) " ++
        "is impossible to satisfy when outputting to the " ++
        "Acacia/Aciacia+ format, since it does not support " ++
        "the weak until nor the release operator.\n" ++
        "Remove at least one of these constraints."

  | negNormalForm && noRelease &&
    (noGlobally || noDerived) && outputFormat == SMV =

      cfgError $
        "The given combination of transformations " ++
        "(negation normal form, no release operators, and " ++
        "no globally / derived operators) " ++
        "is impossible to satisfy when outputting to the " ++
        "SMV format, since it does not support " ++
        "the weak until operator.\n" ++
        "Remove at least one of these constraints."

  | negNormalForm && noGlobally && outputFormat == PSL =

      cfgError $
        "The given combination of transformations " ++
        "(negation normal form and no globally operators)" ++
        "is impossible to satisfy when outputting to the " ++
        "PSL format, since it does not support " ++
        "the weak until and the release operator.\n" ++
        "Remove at least one of these constraints."

  | negNormalForm && noDerived && outputFormat == PSL =

      cfgError $
        "The given combination of transformations " ++
        "(negation normal form and no derived operators)" ++
        "is impossible to satisfy when outputting to the " ++
        "PSL format, since it does not support " ++
        "the release operator.\n" ++
        "Remove at least one of these constraints."

  | negNormalForm && noDerived && outputFormat == UNBEAST =

      cfgError $
        "The given combination of transformations " ++
        "(negation normal form and no derived operators)" ++
        "is impossible to satisfy when outputting to the " ++
        "UNBEAST format, since it does not support " ++
        "the release operator.\n" ++
        "Remove at least one of these constraints."

  | outputFormat == FULL &&
    (isJust owSemantics || isJust owTarget ||
     simplifyWeak || simplifyStrong || negNormalForm ||
     pushGlobally || pushFinally || pushNext ||
     pullGlobally || pullFinally || pullNext ||
     noWeak || noRelease || noFinally || noGlobally ||
     noDerived) =

      cfgError $
        "Applying adaptions is only possible, when transforming to " ++
        "low level backends.\n Returning full TLSF only " ++
        "allows to change parameters."

  | otherwise = return ()

-----------------------------------------------------------------------------

-- | Creates the content of a parsable configuration file restricted
-- to supported configuration file parameters.

instance Convertible Configuration String where
  safeConvert Configuration{..} = return $ unlines
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
    , set "format" $ convert $ outputFormat
    , emptyline
    , comment $ "Specifies the representation mode of the output. " ++
      "Use "
    , comment $ "\"" ++ name ++ " --help\" to check for possible " ++
      "values."
    , set "mode" $ convert $ outputMode
    , emptyline
    , comment $ "Specifies the quote mode of the output. " ++
      "Use "
    , comment $ "\"" ++ name ++ " --help\" to check for possible " ++
      "values."
    , set "quote" $ convert $ quoteMode
    , emptyline
    , comment $ "Specifies the bus delimiter symbol / string. The " ++
      "value has to be "
    , comment "encapsulated into quotation marks."
    , set "bus_delimiter" $ "\"" ++ busDelimiter ++ "\""
    , emptyline
    , comment $ "Specifies the output representation of prime " ++
      "symbols. The value "
    , comment "has to be encapsulated into quotation marks."
    , set "prime_symbol" $ "\"" ++ primeSymbol ++ "\""
    , emptyline
    , comment $ "Specifies the output representation of \"@\"-" ++
      "symbols. The value "
    , comment "has to be encapsulated into quotation marks."
    , set "at_symbol" $ "\"" ++ atSymbol ++ "\""
    , emptyline
    , comment $ "Overwrites the semantics of the input " ++
      "specification. Do not set"
    , comment "to keep the value unchanged."
    , ifJust owSemantics "overwrite_semantics" convert
    , emptyline
    , comment $ "Overwrites the target of the input " ++
      "specification. Do not set"
    , comment "to keep the value unchanged."
    , ifJust owTarget "overwrite_target" convert
    , emptyline
    , comment $ "Either enable or disable weak simplifications on " ++
      "the LTL"
    , comment $ "formula level. Possible values are either \"true\" " ++
      "or \"false\"."
    , set "weak_simplify" $ convert $ CBool simplifyWeak
    , emptyline
    , comment $ "Either enable or disable strong simplifications on " ++
      "the LTL"
    , comment $ "formula level. Possible values are either \"true\" " ++
      "or \"false\"."
    , set "strong_simplify" $ convert $ CBool simplifyStrong
    , emptyline
    , comment $ "Either enable or disable that the resulting " ++
      "formula is"
    , comment "converted into negation normal form. Possible values " ++
      "are"
    , comment "either \"true\" or \"false\"."
    , set "negation_normal_form" $ convert $ CBool negNormalForm
    , emptyline
    , comment $ "Either enable or disable to push globally operators " ++
      "inwards,"
    , comment "i.e., to apply the following equivalence:"
    , comment ""
    , comment "  G (a && b) => (G a) && (G b)"
    , comment ""
    , comment "Possible values are either \"true\" or \"false\"."
    , set "push_globally_inwards" $ convert $ CBool pushGlobally
    , emptyline
    , comment $ "Either enable or disable to push finally operators " ++
      "inwards,"
    , comment "i.e., to apply the following equivalence:"
    , comment ""
    , comment "  F (a || b) => (F a) || (F b)"
    , comment ""
    , comment "Possible values are either \"true\" or \"false\"."
    , set "push_finally_inwards" $ convert $ CBool pushFinally
    , emptyline
    , comment $ "Either enable or disable to next operators " ++
      "inwards, i.e.,"
    , comment "to apply the following equivalences:"
    , comment ""
    , comment "  X (a && b) => (X a) && (X b)"
    , comment "  X (a || b) => (X a) || (X b)"
    , comment ""
    , comment "Possible values are either \"true\" or \"false\"."
    , set "push_next_inwards" $ convert $ CBool  pushNext
    , emptyline
    , comment $ "Either enable or disable to pull globally operators " ++
      "outwards,"
    , comment "i.e., to apply the following equivalence:"
    , comment ""
    , comment "  (G a) && (G b) => G (a && b)"
    , comment ""
    , comment "Possible values are either \"true\" or \"false\"."
    , set "pull_globally_outwards" $ convert $ CBool pullGlobally
    , emptyline
    , comment $ "Either enable or disable to pull finally operators " ++
      "outwards,"
    , comment "i.e., to apply the following equivalence:"
    , comment ""
    , comment "  (F a) || (F b) => F (a || b)"
    , comment ""
    , comment "Possible values are either \"true\" or \"false\"."
    , set "pull_finally_outwards" $ convert $ CBool pullFinally
    , emptyline
    , comment $ "Either enable or disable to pull next operators " ++
      "outwards,"
    , comment "i.e., to apply the following equivalences:"
    , comment ""
    , comment "  (X a) && (X b) => X (a && b)"
    , comment "  (X a) || (X b) => X (a || b)"
    , comment ""
    , comment "Possible values are either \"true\" or \"false\"."
    , set "pull_next_outwards" $ convert $ CBool pullNext
    , emptyline
    , comment $ "Either enable or disable to resolve weak until " ++
      "operators."
    , comment "Possible values are either \"true\" or \"false\"."
    , set "no_weak_until" $ convert $ CBool noWeak
    , emptyline
    , comment $ "Either enable or disable to resolve release " ++
      "operators."
    , comment "Possible values are either \"true\" or \"false\"."
    , set "no_release" $ convert $ CBool noRelease
    , emptyline
    , comment $ "Either enable or disable to resolve finally " ++
      "operators."
    , comment "Possible values are either \"true\" or \"false\"."
    , set "no_finally" $ convert $ CBool noFinally
    , emptyline
    , comment $ "Either enable or disable to resolve globally " ++
      "operators."
    , comment "Possible values are either \"true\" or \"false\"."
    , set "no_globally" $ convert $ CBool noGlobally
    , emptyline
    , comment $ "Either enable or disable to resolve derived " ++
      "operators, i.e.,"
    , comment "weak until, finally, globally, ... . Possible " ++
      "values are"
    , comment "either \"true\" or \"false\"."
    , set "no_derived" $ convert $ CBool noDerived
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

-- | Parses configuration parameters from the content of a
-- configuration file and updates the respective entries in the
-- provided configuration.

update
  :: Configuration -> String -> Either Error Configuration

update c str =
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
      <|> (mParser "quote"
             >>= (\v -> return (\c -> c { quoteMode = v })))
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
             >>= (\(CBool v) -> return (\c -> c { simplifyWeak = v })))
      <|> (mParser "strong_simplify"
             >>= (\(CBool v) -> return (\c -> c { simplifyStrong = v })))
      <|> (mParser "negation_normal_form"
             >>= (\(CBool v) -> return (\c -> c { negNormalForm = v })))
      <|> (mParser "push_globally_inwards"
             >>= (\(CBool v) -> return (\c -> c { pushGlobally = v })))
      <|> (mParser "push_finally_inwards"
             >>= (\(CBool v) -> return (\c -> c { pushFinally = v })))
      <|> (mParser "push_next_inwards"
             >>= (\(CBool v) -> return (\c -> c { pushNext = v })))
      <|> (mParser "pull_globally_outwards"
             >>= (\(CBool v) -> return (\c -> c { pullGlobally = v })))
      <|> (mParser "pull_finally_outwards"
             >>= (\(CBool v) -> return (\c -> c { pullFinally = v })))
      <|> (mParser "pull_next_outwards"
             >>= (\(CBool v) -> return (\c -> c { pullNext = v })))
      <|> (mParser "no_weak_until"
             >>= (\(CBool v) -> return (\c -> c { noWeak = v })))
      <|> (mParser "no_release"
             >>= (\(CBool v) -> return (\c -> c { noRelease = v })))
      <|> (mParser "no_finally"
             >>= (\(CBool v) -> return (\c -> c { noFinally = v })))
      <|> (mParser "no_globally"
             >>= (\(CBool v) -> return (\c -> c { noGlobally = v })))
      <|> (mParser "no_derived"
             >>= (\(CBool v) -> return (\c -> c { noDerived = v })))

    sParser str = do
      keyword str
      op "="
      stringLiteral tokenparser

    mParser str = do
      keyword str
      op "="
      v <- identifier tokenparser
      case safeConvert v of
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
        [ "format", "mode", "quote", "bus_delimiter", "prime_symbol", "at_symbol",
          "overwrite_semantics", "overwrite_target", "weak_simplify",
          "strong_simplify", "negation_normal_form",
          "push_globally_inwards", "push_finally_inwards",
          "push_next_inwards", "pull_globally_outwards",
          "pull_finally_outwards", "pull_next_outwards", "no_weak_until",
           "no_release", "no_finally", "no_globally", "no_derived" ]
  }

-----------------------------------------------------------------------------

newtype CBool = CBool Bool

-----------------------------------------------------------------------------

instance Convertible CBool String where
  safeConvert = return . \case
    (CBool True)  -> "true"
    (CBool False) -> "false"

-----------------------------------------------------------------------------

instance Convertible String CBool where
  safeConvert = \case
    "true"  -> return $ CBool True
    "false" -> return $ CBool False
    str     -> Left ConvertError
      { convSourceValue = str
      , convSourceType = "String"
      , convDestType = "Bool"
      , convErrorMessage = "Unknown value"
      }

-----------------------------------------------------------------------------
