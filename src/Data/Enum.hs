-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Enum
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
-- 
-- Data types to store enum definitions and applications.
-- 
-----------------------------------------------------------------------------

module Data.Enum
    ( EnumDefinition(..)
    , EnumId(..)
    ) where

-----------------------------------------------------------------------------

import Data.Expression
    ( ExprPos
    )

-----------------------------------------------------------------------------

-- | An enumeration definiton consists of a name, the number of
-- entries, the values associated with each entry, the position, a list
-- of entries not explicitly listed and ?.

data EnumDefinition a =
  EnumDefinition
  { eName :: a
  , eSize :: Int
  , eValues :: [(a, ExprPos, [Int -> Either Bool ()])]
  , ePos :: ExprPos
  , eMissing  :: [Int -> Either Bool ()]
  , eDouble :: Maybe ((a,ExprPos), (a, ExprPos), (a,ExprPos), Int -> Either Bool ())
  } 

-----------------------------------------------------------------------------

-- | An @EnumId@ contains all information to uniqely identify an
-- enumaration.

data EnumId a =
  EnumId
  { eIName :: a
  , eIPos :: ExprPos 
  , eISize :: Int
  , eIVName :: a
  , eIValue :: Int -> Bool
  }  

-----------------------------------------------------------------------------             
