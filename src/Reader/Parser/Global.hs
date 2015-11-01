-----------------------------------------------------------------------------
-- |
-- Module      :  Reader.Parser.Global
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
-- 
-- Parser for the GLOBAl section.
-- 
-----------------------------------------------------------------------------

module Reader.Parser.Global
    ( globalParser
    ) where

-----------------------------------------------------------------------------

import Data.Binding
    ( BindExpr(..)
    )
    
import Data.Expression
    ( ExprPos(..)
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
    )  

import Text.Parsec
    ( (<|>)
    , char
    , oneOf
    , sepBy
    , many1
    )
    
import Text.Parsec.String
    ( Parser
    )
    
import Text.Parsec.Token
    ( GenLanguageDef(..)
    , commaSep
    , reservedNames  
    , whiteSpace
    , makeTokenParser
    , reserved
    , braces 
    , reservedOp      
    )  

-----------------------------------------------------------------------------

-- | Parses the GLOBAL section of a specification file and returns the list
-- of parameter bindings and the list of the remaining definitions.

globalParser
  :: Parser ([BindExpr String], [BindExpr String])

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
      return $ Just $ BindExpr x args pos es

    ch = void . char
    br = braces tokenparser
    rOp = reservedOp tokenparser
    (~~) = whiteSpace tokenparser
    keyword = void . reserved tokenparser    

-----------------------------------------------------------------------------
