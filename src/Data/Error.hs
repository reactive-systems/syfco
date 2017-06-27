-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Error
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- Data structures to wrap all contents, that are needed to print nice
-- error messages.
--
-----------------------------------------------------------------------------

{-# LANGUAGE

    LambdaCase
  , RecordWildCards

  #-}

-----------------------------------------------------------------------------

module Data.Error
  ( Error
  , syntaxError
  , runtimeError
  , typeError
  , bindingError
  , conversionError
  , depError
  , cfgError
  , parseError
  , prError
  , prErrPos
  ) where

-----------------------------------------------------------------------------

import Data.Expression
  ( ExprPos(..)
  , SrcPos(..)
  )

import Text.Parsec.Error
  ( ParseError
  )

import System.Exit
  ( exitFailure
  )

import System.IO
  ( hPrint
  , stderr
  )

-----------------------------------------------------------------------------

-- | Unifying data structure.

data Error =
    ErrType TypeError
  | ErrParse ParseError
  | ErrBnd BindingError
  | ErrDep DependencyError
  | ErrSyntax SyntaxError
  | ErrRunT RunTimeError
  | ErrConv ConvError
  | ErrCfg CfgError

-----------------------------------------------------------------------------

data TypeError =
  TypeError
  { errTPos :: ExprPos
  , errTMsgs :: [String]
  } deriving (Eq, Ord)

-----------------------------------------------------------------------------

data BindingError =
  BindingError
  { errBPos :: ExprPos
  , errBMsgs :: [String]
  } deriving (Eq, Ord)

-----------------------------------------------------------------------------

data DependencyError =
  DependencyError
  { errDPos :: ExprPos
  , errDMsgs :: [String]
  } deriving (Eq, Ord)

-----------------------------------------------------------------------------

data SyntaxError =
  SyntaxError
  { errSPos :: ExprPos
  , errSMsgs :: [String]
  } deriving (Eq, Ord)

-----------------------------------------------------------------------------

data RunTimeError =
  RunTimeError
  { errRPos :: ExprPos
  , errRMsgs :: [String]
  } deriving (Eq, Ord)

-----------------------------------------------------------------------------

data ConvError =
  ConvError
  { title :: String
  , cmsg :: String
  } deriving (Eq, Ord)

-----------------------------------------------------------------------------

data CfgError =
  ConfigError
  { fmsg :: String
  } deriving (Eq, Ord)

-----------------------------------------------------------------------------

instance Show Error where
  show = \case
    ErrParse x                 -> show x
    ErrType TypeError{..}      -> pr "Type Error" errTPos errTMsgs
    ErrBnd BindingError{..}    -> pr "Binding Error" errBPos errBMsgs
    ErrDep DependencyError{..} -> pr "Dependency Error" errDPos errDMsgs
    ErrSyntax SyntaxError{..}  -> pr "Syntax Error" errSPos errSMsgs
    ErrRunT RunTimeError{..}   -> pr "Runtime Error" errRPos errRMsgs
    ErrCfg ConfigError{..}     -> "\"Error\":\n" ++ fmsg
    ErrConv ConvError{..}      -> "\"Conversion Error\": " ++ title ++
                                 "\n" ++ cmsg

    where
      pr errname pos msgs =
        "\"" ++ errname ++ "\" (" ++ prErrPos pos ++ "):\n" ++ concat msgs

-----------------------------------------------------------------------------

-- | Use this error constructor, if some sytax related misbehavior is
-- detected.

syntaxError
  :: ExprPos -> String -> Either Error a

syntaxError pos msg = Left $ ErrSyntax $ SyntaxError pos [msg]

-----------------------------------------------------------------------------

-- | Use this error constructor, if some runtime execution fails.

runtimeError
  :: ExprPos -> String -> Either Error a

runtimeError pos msg = Left $ ErrRunT $ RunTimeError pos [msg]

-----------------------------------------------------------------------------

-- | Use this error constructor, if some type related misbehavior is
-- detected.

typeError
  :: ExprPos -> String -> Either Error a

typeError pos msg = Left $ ErrType $ TypeError pos [msg]

-----------------------------------------------------------------------------

-- | Use this error constructor, if some identifier binding related
-- misbehavior is detected.

bindingError
  :: ExprPos -> String -> Either Error a

bindingError pos msg = Left $ ErrBnd $ BindingError pos [msg]

-----------------------------------------------------------------------------

-- | Use this error constructor, if some misbehavior concerning dependencies
-- between identifiers is detected.

depError
  :: ExprPos -> String -> Either Error a

depError pos msg = Left $ ErrDep $ DependencyError pos [msg]


-----------------------------------------------------------------------------

-- | Use this error constructor, if some unresolvable inconsistency in the
-- configuration exists.

cfgError
  :: String -> Either Error a

cfgError msg = Left $ ErrCfg $ ConfigError msg

-----------------------------------------------------------------------------

-- | Use this error constructor, if an invalid command line setting is
-- detected.

conversionError
  :: String -> String -> Either Error a

conversionError t msg = Left $ ErrConv $ ConvError t msg

-----------------------------------------------------------------------------

-- | Use this error constructor, whenever a parser fails.

parseError
  :: ParseError -> Either Error a

parseError err = Left $ ErrParse err

-----------------------------------------------------------------------------

-- | Prints an error to STDERR and then terminates the program.

prError
  :: Error -> IO a

prError err = do
  hPrint stderr $ show err
  exitFailure

-----------------------------------------------------------------------------

-- | Prints the position of an error related token.

prErrPos
  :: ExprPos -> String

prErrPos pos =
  let
    bl = srcLine $ srcBegin pos
    bc = srcColumn $ srcBegin pos
    el = srcLine $ srcEnd pos
    ec = srcColumn $ srcEnd pos
  in
    "line " ++ show bl ++ "," ++
    "column " ++ show bc ++
    if bl == el
    then " - " ++ show ec
    else " - line " ++ show el ++ ", column " ++ show ec

-----------------------------------------------------------------------------
