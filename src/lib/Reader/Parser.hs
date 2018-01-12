-----------------------------------------------------------------------------
-- |
-- Module      :  Reader.Parser
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- Parsing module containing all neccessary parsers.
--
-----------------------------------------------------------------------------

module Reader.Parser
  ( parse
  ) where

-----------------------------------------------------------------------------

import Data.Enum
  ( EnumDefinition(..)
  )

import Data.Error
  ( Error
  , parseError
  )

import Reader.Error
  ( errEnumConflict
  )

import Reader.Parser.Data
  ( Specification(..)
  )

import Reader.Parser.Info
  ( infoParser
  )

import Reader.Parser.Global
  ( globalParser
  )

import Reader.Parser.Component
  ( componentParser
  )

import Text.Parsec
  ( (<|>)
  )

import qualified Text.Parsec as P
  ( parse
  )

import Text.Parsec.String
  ( Parser
  )

-----------------------------------------------------------------------------

-- | @parseSpecification str @ parses a specification from the string @str@.

parse
  :: String -> Either Error Specification

parse str =
  case P.parse specificationParser "Syntax Error" str of
    Left err -> parseError err
    Right x  -> do
      mapM_ checkEnum $ enumerations x
      return x

-----------------------------------------------------------------------------

specificationParser
  :: Parser Specification

specificationParser = do
  (i,d,s,r,a) <- infoParser
  (ps,vs,ms) <- globalParser <|> return ([],[],[])
  (is,os,es,ss,rs,as,ns,gs) <- componentParser

  return Specification
    { title = i
    , description = d
    , semantics = s
    , target = r
    , tags = a
    , enumerations = ms
    , parameters = ps
    , definitions = vs
    , inputs = is
    , outputs = os
    , initially = es
    , preset = ss
    , requirements = rs
    , assumptions = as
    , invariants = ns
    , guarantees = gs
    }

-----------------------------------------------------------------------------

checkEnum
  :: EnumDefinition String -> Either Error ()

checkEnum e = case eDouble e of
  Just ((m,p),(x,_),(y,_),f) -> errEnumConflict m x y (toStr (eSize e) f) p
  Nothing                    -> return ()

  where
    toStr n f = map (toS . f) [0,1..n-1]

    toS (Right ())    = '*'
    toS (Left True)  = '1'
    toS (Left False) = '0'

-----------------------------------------------------------------------------
