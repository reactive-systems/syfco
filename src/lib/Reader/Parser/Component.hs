-----------------------------------------------------------------------------
-- |
-- Module      :  Reader.Parser.Component
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- Parser for the MAIN section.
--
-----------------------------------------------------------------------------

module Reader.Parser.Component
  ( componentParser
  ) where

-----------------------------------------------------------------------------

import Text.Parsec
  ( (<|>)
  , many
  , char
  , sepBy
  , oneOf
  )

import Text.Parsec.String
  ( Parser
  )

import Text.Parsec.Token
  ( GenLanguageDef(..)
  , makeTokenParser
  , braces
  , reservedOp
  , whiteSpace
  , reserved
  )

import Data.Types
  ( SignalDecType(..)
  )

import Data.Expression
  ( Expr(..)
  , ExprPos(..)
  )

import Reader.Parser.Data
  ( globalDef
  )

import Reader.Parser.Utils
  ( identifier
  , getPos
  )

import Reader.Parser.Expression
  ( exprParser
  )

import Data.Maybe
  ( catMaybes
  )

import Control.Monad
  ( void
  , liftM
  )

-----------------------------------------------------------------------------

data Component =
  Component
  { inputs :: [SignalDecType String]
  , outputs :: [SignalDecType String]
  , initially :: [Expr String]
  , preset :: [Expr String]
  , requirements :: [Expr String]
  , assumptions :: [Expr String]
  , invariants :: [Expr String]
  , guarantees :: [Expr String]
  }

-----------------------------------------------------------------------------

-- | Parses the MAIN section of a specification file. It returns:
--
--     * the input signals of the specification
--
--     * the output signals of the specification
--
--     * the initial configuration of the inputs
--
--     * the initial configuration of the outputs
--
--     * the requirements of the specification
--
--     * the assumptions of the specification
--
--     * the invariants of the specification
--
--     * the guarantees of the specification

componentParser
  :: Parser ([SignalDecType String], [SignalDecType String],
            [Expr String], [Expr String], [Expr String],
            [Expr String], [Expr String], [Expr String])

componentParser = do
  keyword "MAIN"
  xs <- br $ many $ componentContentParser
        Component
        { inputs = []
        , outputs = []
        , initially = []
        , preset = []
        , requirements = []
        , assumptions = []
        , invariants = []
        , guarantees = []
        }

  return
    ( concatMap inputs xs
    , concatMap outputs xs
    , concatMap initially xs
    , concatMap preset xs
    , concatMap requirements xs
    , concatMap assumptions xs
    , concatMap invariants xs
    , concatMap guarantees xs )

  where
    tokenparser =
      makeTokenParser globalDef
      { opStart = oneOf "=;"
      , opLetter = oneOf "=;"
      , reservedOpNames = [ "=", ";" ]
      , reservedNames =
          [ "MAIN"
          , "INPUTS"
          , "OUTPUTS"
          , "INITIALLY"
          , "PRESET"
          , "ASSUME"
          , "ASSUMPTIONS"
          , "REQUIRE"
          , "REQUIREMENTS"
          , "ASSERT"
          , "INVARIANTS"
          , "GUARANTEE"
          , "GUARANTEES"
          ]
      }

    componentContentParser c =
          (sectionParser "INPUTS" signalParser
             >>= \x -> return c { inputs = x       })
      <|> (sectionParser "OUTPUTS" signalParser
             >>= \x -> return c { outputs = x      })
      <|> (sectionParser "INITIALLY" exprParser
             >>= \x -> return c { initially = x    })
      <|> (sectionParser "PRESET" exprParser
             >>= \x -> return c { preset = x       })
      <|> (sectionParser "REQUIRE" exprParser
             >>= \x -> return c { requirements = x })
      <|> (sectionParser "REQUIREMENTS" exprParser
             >>= \x -> return c { requirements = x })
      <|> (sectionParser "ASSUME" exprParser
             >>= \x -> return c { assumptions = x  })
      <|> (sectionParser "ASSUMPTIONS" exprParser
             >>= \x -> return c { assumptions = x  })
      <|> (sectionParser "ASSERT" exprParser
             >>= \x -> return c { invariants = x   })
      <|> (sectionParser "INVARIANTS"  exprParser
             >>= \x -> return c { invariants = x   })
      <|> (sectionParser "GUARANTEE"  exprParser
             >>= \x -> return c { guarantees = x   })
      <|> (sectionParser "GUARANTEES"  exprParser
             >>= \x -> return c { guarantees = x   })

    signalParser = do
      (x,pos) <- identifier (~~)
      typedBusParser x pos
        <|> busParser x pos
        <|> return (SDSingle (x,pos))

    busParser x pos = do
      ch '['; (~~)
      e <- exprParser
      ch ']'; p <- getPos; (~~)
      return $ SDBus (x,(ExprPos (srcBegin pos) p)) e

    typedBusParser x pos = do
      (y,p) <- identifier (~~)
      return $ SDEnum (y,p) (x,pos)

    sectionParser x p = do
      keyword x
      xs <- br $ sepBy (nonEmptyParser p) $ rOp ";"
      return $ catMaybes xs

    nonEmptyParser p =
      liftM return p <|> return Nothing

    ch = void . char
    br = braces tokenparser
    rOp = reservedOp tokenparser
    (~~) = whiteSpace tokenparser
    keyword = void . reserved tokenparser

-----------------------------------------------------------------------------
