module Reader.Parser.Global where

---

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token hiding (identifier)

import Data.Binding
import Data.Expression
import Reader.Parser.Data
import Reader.Parser.Utils
import Reader.Parser.Expression

import Data.Maybe 
import Control.Monad 

---

globalParser
  :: Parser ([Bind Expr String], [Bind Expr String])

globalParser = do
  keyword "GLOBAL"
  ch '{'; (~~)
  globalContentParser [] []
        
  where
    tokenparser =
      makeTokenParser globalDef
      { opStart = oneOf "=;"
      , opLetter = oneOf "=;"
      , reservedOpNames = [ "=", ";" ]
      , reservedNames =
          [ "GLOBAL"
          , "PARAMETERS"
          , "DEFINITIONS"
          ]
      }
    
    globalContentParser ps gs =
          do { keyword "PARAMETERS"; x <- sectionParser; globalContentParser (ps ++ x) gs }
      <|> do { keyword "DEFINITIONS"; x <- sectionParser; globalContentParser ps (gs ++ x) }
      <|> do { ch '}'; (~~); return (ps,gs) } 

    sectionParser = do
      xs <- br $ sepBy assignmentParser $ rOp ";"
      return $ catMaybes xs    

    assignmentParser =
          nonemptyAssignmentParser
      <|> return Nothing
    
    nonemptyAssignmentParser = do
      (x,pos) <- identifier (~~)
      argumentsParser x pos <|> reminderParser x [] pos

    argumentsParser x pos = do
      ch '('; (~~)
      args <- commaSep tokenparser $ identifier (~~)
      ch ')'; p <- getPos; (~~)
      reminderParser x args $ ExprPos (srcBegin pos) p
      
    reminderParser x args pos = do
      rOp "="            
      es <- many1 exprParser
      return $ Just $ Bind x args pos es

    ch = void . char
    br = braces tokenparser
    rOp = reservedOp tokenparser
    (~~) = whiteSpace tokenparser
    keyword = void . reserved tokenparser    

---
