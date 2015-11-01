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

import Data.Error
    ( Error
    , argsError
    , parseError  
    )   

import Data.Binding
    ( BindExpr(..)  
    )
    
import Data.Expression
    ( Expr(..)
    , Expr'(..)
    )   

import Reader.Parser.Data
    ( Specification(..)
    , globalDef  
    )
    
import Reader.Parser.Info
    ( semanticsParser
    , targetParser
    , infoParser  
    )
    
import Reader.Parser.Global
    ( globalParser
    )
    
import Reader.Parser.Component
    ( componentParser
    )  

import Data.List
    ( find
    )
    
import Control.Monad
    ( foldM
    , void  
    )  

import Text.Parsec
    ( (<|>)
    , char
    , many1
    , eof
    , digit  
    )

import qualified Text.Parsec as P
    ( parse
    )  
      
import Text.Parsec.String
    ( Parser
    )
    
import Text.Parsec.Token
    ( makeTokenParser
    , identifier  
    )      

-----------------------------------------------------------------------------

-- | @parseSpecification str m t ps@ parses a specification from the string
-- @str@ and adapts the result according the overwriting options. Thereby,
-- @m@ may contain a new semantics setting, @t@ may contain a new target
-- setting and @ps@ is the list over parameters to overwrite.

parse
  :: String -> Maybe String -> Maybe String -> [String]
      -> Either Error Specification

parse str m t ps = 
  case executeParsers of
    Left err -> parseError err
    Right x  -> do
      pps <- mapM parseParameter ps
      foldM overwriteParameter x pps

  where
    executeParsers = do
      x <- P.parse specificationParser "Syntax Error" str
      y <- case m of
        Nothing -> return $ semantics x
        Just sm -> P.parse semanticsParser "Overwrite Semantics Error" sm
      z <- case t of
        Nothing -> return $ target x
        Just st -> P.parse targetParser "Overwrite Target Error" st
      return x {
        semantics = y,
        target = z
        }

-----------------------------------------------------------------------------

specificationParser
  :: Parser Specification

specificationParser = do
  (i,d,s,r,a) <- infoParser 
  (ps,vs) <- globalParser <|> return ([],[])
  (is,os,as,ns,gs) <- componentParser

  return Specification
    { title = i
    , description = d
    , semantics = s
    , target = r
    , tags = a
    , parameters = ps
    , definitions = vs
    , inputs = is
    , outputs = os
    , assumptions = as
    , invariants = ns
    , guarantees = gs
    }
             
-----------------------------------------------------------------------------

parseParameter
  :: String -> Either Error (String, Int)

parseParameter str =
  case P.parse parameterParser "Overwrite Parameter Error" str of
    Left err    -> parseError err
    Right (s,v) -> return (s,v)

-----------------------------------------------------------------------------

parameterParser
  :: Parser (String,Int)

parameterParser = do
  name <- identifier $ makeTokenParser globalDef
  void $ char '='
  x <- many1 digit
  eof
  return (name, read x)

-----------------------------------------------------------------------------

overwriteParameter
  :: Specification -> (String,Int) -> Either Error Specification

overwriteParameter s (n,v) = case find ((== n) . bIdent) $ parameters s of
  Nothing -> argsError $ "Specification has no parameter: " ++ n 
  Just b  -> do
    let b' = b {
          bVal = if null $ bVal b
                 then []
                 else [ Expr (BaseCon v) $ srcPos $ head $ bVal b ]
          }
    return s { parameters = map (replace b') $ parameters s }

  where
    replace b' b =
      if bIdent b == bIdent b'
      then b' else b

-----------------------------------------------------------------------------

