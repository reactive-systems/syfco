-----------------------------------------------------------------------------
-- |
-- Module      :  Reader.Parser.Utils
-- Description :  Functions shared among the different parsers
-- License     :  MIT (see the LICENSE file)
-- 
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
-- 
-- Functions shared among the different parsers
-- 
-----------------------------------------------------------------------------

module Reader.Parser.Utils
    ( stringParser
    , identifier
    , positionParser
    , getPos
    ) where

-----------------------------------------------------------------------------

import Data.Expression
    ( ExprPos(..)
    , SrcPos(..)  
    )
    
import Reader.Parser.Data
    ( globalDef
    )  

import Control.Monad
    ( void
    )
    
import Data.Functor.Identity
    ( Identity
    )

import Text.Parsec
    ( ParsecT
    , Stream      
    , (<|>)
    , char
    , many
    , anyChar  
    , noneOf
    , getPosition
    , sourceLine
    , sourceColumn  
    )
    
import Text.Parsec.String
    ( Parser
    )
    
import Text.Parsec.Token 
    ( identStart
    , identLetter
    ) 

-----------------------------------------------------------------------------

-- | Parses a string surrounded by double quotation marks.
-- Double quotation marks inside the string have to be escabed by a backslash.
-- The string may contain special characters like new line or tabs.

stringParser
  :: Parser String
     
stringParser = do
    void $ char '"'
    xs <- many character
    void $ char '"'
    return $ concat xs
    
  where
    character =
      escaped <|> nonescaped

    escaped = do
      d <- char '\\'
      c <- anyChar
      case c of
        '"' -> return [c]
        _   -> return [d,c]

    nonescaped = do
      c <- noneOf "\""
      return [c]

-----------------------------------------------------------------------------

-- | @identifier wp@ is an exteded version of its equivalent exported by
-- 'Text.Parser.Token', which additionally stores the starting source position
-- and the ending source position. The parser @wp@ is assumed to parse the
-- subsequent whitespace after the identifier.

identifier
  :: ParsecT String u Identity () -> ParsecT String u Identity (String, ExprPos)

identifier wp = positionParser wp $ do
    x <- identStart globalDef
    xr <- many $ identLetter globalDef
    return (x:xr)

-----------------------------------------------------------------------------

-- | @positionParser wp p@ parses the same as parser @p@, but additionally
-- returns the starting and ending source positions of the result parsed by
-- @p@. The parser @wp@ is assumed to parse the subsequent whitespace after
-- @p@ has been invoked.

positionParser
 :: (Stream s m t) => ParsecT s u m () -> ParsecT s u m a
    -> ParsecT s u m (a,ExprPos)

positionParser wp p = do
  x <- getPos; e <- p
  y <- getPos; wp
  return (e,ExprPos x y)

-----------------------------------------------------------------------------

-- | Return the current position of the parser in the source file.

getPos
 :: (Stream s m t) => ParsecT s u m SrcPos

getPos = do
  x <- getPosition
  return $ SrcPos (sourceLine x) (sourceColumn x)      
      
-----------------------------------------------------------------------------
