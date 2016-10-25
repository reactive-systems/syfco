-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Utils
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- Utilization functions and classes for data types.
--
-----------------------------------------------------------------------------

module Data.Utils
  ( MachinePrintable(..)
  ) where

-----------------------------------------------------------------------------

import Data.List
  ( find
  )

import Data.Error
  ( Error
  , conversionError
  )

import Generics.Deriving.Enum
  ( GEnum
  , genum
  )

-----------------------------------------------------------------------------

-- | Allows to print some internal data structure in a machine
-- readable version. If the data structure is additionally
-- enumeratable, the class also provied an automatic parser. Note that
-- the printed version has to be unique for this case.

class MachinePrintable a where
  mprint :: a -> String

  mparse :: GEnum a => String -> Either Error a
  mparse str =
    let xs = map (\x -> (x,mprint x)) genum
    in case find ((== str) . snd) xs of
      Just (fmt,_) -> return fmt
      Nothing      -> conversionError "Unknown Input" str

-----------------------------------------------------------------------------

-- | MachinePrintable instance for bools.

instance MachinePrintable Bool where
  mprint b =
    if b then "true" else "false"

-----------------------------------------------------------------------------
