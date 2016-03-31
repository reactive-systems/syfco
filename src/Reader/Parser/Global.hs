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

import Data.List
    ( find
    )
    
import Data.Enum
    ( EnumDefinition(..)
    )

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

import Data.Either
    ( partitionEithers
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
    , many
    , count  
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

import Control.Exception
    ( assert
    )  

import qualified Data.Array.IArray as A    

-----------------------------------------------------------------------------

-- | Parses the GLOBAL section of a specification file and returns the list
-- of parameter bindings and the list of the remaining definitions.

globalParser
  :: Parser ([BindExpr String], [BindExpr String], [EnumDefinition String])

globalParser = do
  keyword "GLOBAL"
  ch '{'; (~~)
  globalContentParser [] [] []
        
  where
    tokenparser =
      makeTokenParser globalDef
      { opStart = oneOf "=;:"
      , opLetter = oneOf "=;:"
      , reservedOpNames = [ "=", ";", ":"]
      , reservedNames =
          [ "GLOBAL"
          , "PARAMETERS"
          , "DEFINITIONS"
          , "enum"  
          ]
      }
    
    globalContentParser ps gs ns =
          do { ch '}'; (~~); return (ps,gs,ns) }       
      <|> do { keyword "PARAMETERS"; x <- sectionParser;
               globalContentParser (ps ++ x) gs ns }
      <|> do { keyword "DEFINITIONS"; (x,y) <- sectionEnumParser;
               globalContentParser ps (gs ++ x) (ns ++ y) }

    sectionParser = do
      xs <- br $ sepBy assignmentParser $ rOp ";"
      return $ catMaybes xs    

    sectionEnumParser = do
      xs <- br $ sepBy assignmentEnumParser $ rOp ";"
      return $ partitionEithers $ catMaybes xs    

    assignmentParser =
          nonemptyAssignmentParser
      <|> return Nothing

    assignmentEnumParser =
          enumParser 
      <|> nonemptyAssignmentEnumParser
      <|> return Nothing      
    
    nonemptyAssignmentParser = do
      (x,pos) <- identifier (~~)
      argumentsParser x pos <|> reminderParser x [] pos

    nonemptyAssignmentEnumParser = do
      (x,pos) <- identifier (~~)
      argumentsEnumParser x pos <|> reminderEnumParser x [] pos      

    enumParser = do
      keyword "enum"
      (x,pos) <- identifier (~~)
      rOp "="
      (v,n) <- enumVParser
      vr <- many (enumVParserL n)
      let (d,m) = analyze n (x,pos) (v:vr)
      return $ Just $ Right EnumDefinition {
        eName = x,
        eSize = n,
        ePos = pos,
        eValues = v : vr,
        eMissing = m,
        eDouble = d
        }

    analyze n e vs = let
        as = [ (v, appears v vs) | v <- allValues [[]] n ]
        ms = map (toMap n . fst) $ filter (null . snd) as
      in case find ((> 1) . length . snd) as of
        Nothing        -> (Nothing, ms)
        Just (v,x:y:_) -> (Just (e,x,y,toMap n v), ms)
        _              -> assert False undefined

    appears vs = foldl (appV vs) []

    appV vs a (m,p,xs) = foldl (appF vs m p) a xs

    appF vs m p a f
      | cmpF f (reverse vs) = (m,p) :a
      | otherwise           = a

    cmpF _ []     = True
    cmpF f (x:xr) = case f $ length xr of
      Right () -> cmpF f xr
      v       -> v == x && cmpF f xr

    allValues a n
      | n <= 0     = a
      | otherwise =
        allValues (map (Left True :) a ++
                   map (Left False :) a) (n-1)
   
    enumVParser = do
      (x,p) <- identifier (~~)
      rOp ":"
      v <- valueParser
      vr <- many $ valueSepParserL (length v)
      (~~)
      let fs = map (toMap (length v)) (v:vr)
      return ((x, p, fs), length v)

    enumVParserL n = do
      (x,p) <- identifier (~~)
      rOp ":"
      v <- valueParserL n
      vr <- many $ valueSepParserL n
      (~~)
      let fs = map (toMap n) (v:vr)
      return (x, p, fs)

    toMap n xs =
      let
        a :: A.Array Int (Either Bool ())
        a = A.array (0,n-1) $ zip [0,1..n-1] xs
      in
        (a A.!)

    valueParser = many1 bitParser      

    valueParserL n = count n bitParser          

    valueSepParserL n =
      ch ',' >> (~~) >> valueParserL n

    bitParser =
          do { ch '0'; return $ Left False }
      <|> do { ch '1'; return $ Left True }
      <|> do { ch '*'; return $ Right () }

    argumentsParser x pos = do
      ch '('; (~~)
      args <- commaSep tokenparser $ identifier (~~)
      ch ')'; p <- getPos; (~~)
      reminderParser x args $ ExprPos (srcBegin pos) p
      
    reminderParser x args pos = do
      rOp "="            
      es <- many1 exprParser
      return $ Just $ BindExpr x args pos es

    argumentsEnumParser x pos = do
      ch '('; (~~)
      args <- commaSep tokenparser $ identifier (~~)
      ch ')'; p <- getPos; (~~)
      reminderEnumParser x args $ ExprPos (srcBegin pos) p
      
    reminderEnumParser x args pos = do
      rOp "="            
      es <- many1 exprParser
      return $ Just $ Left $ BindExpr x args pos es      

    ch = void . char
    br = braces tokenparser
    rOp = reservedOp tokenparser
    (~~) = whiteSpace tokenparser
    keyword = void . reserved tokenparser    

-----------------------------------------------------------------------------
