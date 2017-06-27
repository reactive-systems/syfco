 -----------------------------------------------------------------------------
-- |
-- Module      :  Parse
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- Parse class to parse a type instance from a machine readable string.
--
-----------------------------------------------------------------------------

{-# LANGUAGE

    LambdaCase

  #-}

-----------------------------------------------------------------------------

module Parse
  ( Parse(..)
  ) where

-----------------------------------------------------------------------------

import Print
  ( Print
  )

import Data.Error
  ( Error
  , conversionError
  )

-----------------------------------------------------------------------------

-- | The @Parse@ class supports to convert strings back to the
-- respective value of the given type. The class requires that
--
-- @ toString . fromString = id @

class Print a => Parse a where
  fromString :: String -> Either Error a

-----------------------------------------------------------------------------

instance Parse Bool where
  fromString = \case
    "true"  -> return True
    "false" -> return False
    str     -> conversionError
                "Boolean Parser"
                ("Conversion failed: " ++ str)

-----------------------------------------------------------------------------
