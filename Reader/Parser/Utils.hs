module Reader.Parser.Utils
       ( stringParser
       , identifier
       , positionParser
       , getPos
       ) where

---

import Data.Expression
import Reader.Parser.Data

import Control.Monad
import Data.Functor.Identity

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token hiding (identifier)

---

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

---

identifier
  :: ParsecT String u Identity () -> ParsecT String u Identity (String, ExprPos)

identifier wp = positionParser wp $ do
    x <- identStart globalDef
    xr <- many $ identLetter globalDef
    return (x:xr)

---

positionParser
 :: (Stream s m t) => ParsecT s u m () -> ParsecT s u m a -> ParsecT s u m (a,ExprPos)

positionParser wp p = do
  x <- getPos; e <- p
  y <- getPos; wp
  return (e,ExprPos x y)

---

getPos
 :: (Stream s m t) => ParsecT s u m SrcPos

getPos = do
  x <- getPosition
  return $ SrcPos (sourceLine x) (sourceColumn x)      
      
---
