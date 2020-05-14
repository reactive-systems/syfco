-----------------------------------------------------------------------------
-- |
-- Module      :  Writer.Data
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- Common data used by the writer module.
--
-----------------------------------------------------------------------------

{-# LANGUAGE

    LambdaCase
  , MultiParamTypeClasses
  , TypeSynonymInstances
  , FlexibleInstances

  #-}

-----------------------------------------------------------------------------

module Writer.Data
  ( WriteMode(..)
  , QuoteMode(..)
  , OperatorConfig(..)
  , UnaryOperator(..)
  , BinaryOperator(..)
  , Unsupported(..)
  , Assoc(..)
  ) where

-----------------------------------------------------------------------------

import Data.Convertible
  ( Convertible(..)
  , ConvertError(..)
  )

-----------------------------------------------------------------------------

-- | There are two writing modes currently supported:

data WriteMode =
    Pretty
    -- ^ pretty printing, producing a well readable, minimal ouptut
  | Fully
    -- ^ fully paranthesized printing, producing fully parenthesized
    --   expressions
  deriving (Eq, Ord)

-----------------------------------------------------------------------------

-- | There are two quoting modes currently supported:

data QuoteMode =
    NoQuotes
    -- ^ do not quote identifiers
  | DoubleQuotes
    -- ^ quote identifiers using "
  deriving (Eq, Ord)

-----------------------------------------------------------------------------

instance Convertible WriteMode String where
  safeConvert = return . \case
    Pretty -> "pretty"
    Fully  -> "fully"

-----------------------------------------------------------------------------

instance Convertible String WriteMode where
  safeConvert = \case
    "pretty" -> return Pretty
    "fully"  -> return Fully
    str      -> Left ConvertError
      { convSourceValue = str
      , convSourceType = "String"
      , convDestType = "WriteMode"
      , convErrorMessage = "Unknown mode"
      }

-----------------------------------------------------------------------------

instance Convertible QuoteMode String where
  safeConvert = return . \case
    NoQuotes -> "none"
    DoubleQuotes  -> "double"

-----------------------------------------------------------------------------

instance Convertible String QuoteMode where
  safeConvert = \case
    "none" -> return NoQuotes
    "double"  -> return DoubleQuotes
    str      -> Left ConvertError
      { convSourceValue = str
      , convSourceType = "String"
      , convDestType = "QuoteMode"
      , convErrorMessage = "Unknown quote mode"
      }

-----------------------------------------------------------------------------

-- | Associativity type to distinguis left associative operators from
-- right associative operators

data Assoc =
    AssocLeft
  | AssocRight
  deriving (Eq)

-----------------------------------------------------------------------------

-- | A unary operator can be set up by providing a name and its precedence.

data UnaryOperator =
  UnaryOp
    { uopName :: String
    , uopPrecedence :: Int
    }
  | UnaryOpUnsupported
  deriving (Eq)

-----------------------------------------------------------------------------

-- | A binary operator can be set up by a name, its precedencs and its
-- associativity.

data BinaryOperator =
  BinaryOp
    { bopName :: String
    , bopPrecedence :: Int
    , bopAssoc :: Assoc
    }
  | BinaryOpUnsupported
  deriving (Eq)

-----------------------------------------------------------------------------

-- | A simple expression printer can be set up using the function
-- 'printFormula' from 'Writer.Pretty'. The bundle the specific
-- operator names, their precedence and their associativity, the data
-- structure @OperatorNames@ is used.
--
-- Thereby, constants as True and False are given by Strings and unary
-- operators are given by their name their precedence. For binary
-- operators, additionally the associativity has to be defined.
--
-- The precedence is given by an Integer, where a lower value means
-- higher precedence. If the same value is used for multiple
-- operators, their precedence is treated equally.
--
-- The associativity is either 'AssocLeft' or 'AssocRight'.
--
-- Unsupported Operators can be disabled using 'UnaryOpUnsupported' or
-- 'BinaryOpUnsupported', respectively.

data OperatorConfig =
  OperatorConfig
  { tTrue :: String
  , fFalse :: String
  , opNot :: UnaryOperator
  , opAnd :: BinaryOperator
  , opOr :: BinaryOperator
  , opImplies :: BinaryOperator
  , opEquiv :: BinaryOperator
  , opNext :: UnaryOperator
  , opPrevious :: UnaryOperator
  , opFinally :: UnaryOperator
  , opGlobally :: UnaryOperator
  , opHistorically :: UnaryOperator
  , opOnce :: UnaryOperator
  , opUntil :: BinaryOperator
  , opRelease :: BinaryOperator
  , opWeak :: BinaryOperator
  , opSince :: BinaryOperator
  , opTriggered :: BinaryOperator
  } deriving (Eq)

-----------------------------------------------------------------------------

-- | Unification class to check whether an operator is unsupported or not.

class Unsupported a where
  unsupported :: a -> Bool

instance Unsupported UnaryOperator where
  unsupported = (== UnaryOpUnsupported)

instance Unsupported BinaryOperator where
  unsupported = (== BinaryOpUnsupported)

-----------------------------------------------------------------------------
