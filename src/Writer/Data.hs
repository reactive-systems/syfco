-----------------------------------------------------------------------------
-- |
-- Module      :  Writer.Data
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
-- 
-- Common data used by the writer module.
-- 
-----------------------------------------------------------------------------

module Writer.Data
    ( WriteContents(..)
    , WriteMode(..)
    , OperatorNames(..)
    ) where

-----------------------------------------------------------------------------

-- | The data type @WriteContents@ wraps the result of a writer, namely the
-- main specification content and depending on the writer, some additional
-- content of a partition file.

data WriteContents =
  WriteContents
  { mainFile :: String
  , partitionFile :: Maybe String
  }

-----------------------------------------------------------------------------

-- | There are two writing modes currently supported:
-- 
--     * pretty printing, producing a well readable minimal ouptut
-- 
--     * fully paranthesized printing, producing fully parenthesized
--       expressions

data WriteMode =
    Pretty
  | Fully
  deriving (Eq, Show)

-----------------------------------------------------------------------------

-- | A simple expression printer can be set up using the function 'pretty'
-- from 'Writer.Pretty'. The bundle the specific operator names, the data
-- structure @OperatorNames@ is used.

data OperatorNames =
  OperatorNames
  { opTrue :: String
  , opFalse :: String
  , opNot :: String
  , opAnd :: String
  , opOr :: String
  , opImplies :: String
  , opEquiv :: String
  , opNext :: String
  , opFinally :: String
  , opGlobally :: String
  , opUntil :: String
  , opRelease :: String
  , opWeak :: String
  }

-----------------------------------------------------------------------------
