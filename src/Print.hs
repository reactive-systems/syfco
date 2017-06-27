-----------------------------------------------------------------------------
-- |
-- Module      :  Print
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- Print class to create a machine readable string from a type instance.
--
-----------------------------------------------------------------------------

module Print
  ( Print(..)
  ) where

-----------------------------------------------------------------------------

-- | The @Print@ class supports conversion to 'String' via the
-- 'toString' function.

class Print a where
  toString :: a -> String

-----------------------------------------------------------------------------

instance Print Bool where
  toString b
    | b         = "true"
    | otherwise = "false"

-----------------------------------------------------------------------------
