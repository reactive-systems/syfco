module Reader.Parser where

---

import Data.Error
import Data.Binding
import Data.Expression

import Reader.Parser.Data
import Reader.Parser.Info
import Reader.Parser.Global
import Reader.Parser.Component

import Data.List (find)
import Control.Monad

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token 

---

parseSpecification
  :: String -> Maybe String -> Maybe String -> [String] -> Either Error Specification

parseSpecification str m t ps = 
  case parse specificationParser "Syntax Error" str of
    Left x  -> Left $ ErrParse x
    Right x -> do
      x' <- case m of
        Nothing -> return x
        Just sm -> case parse semanticsParser "Overwrite Semantics Error" sm of
          Left y  -> Left $ ErrParse y
          Right y -> return x { semantics = y }
      x'' <- case t of
        Nothing -> return x'
        Just st -> case parse targetParser "Overwrite Target Error" st of
          Left y -> Left $ ErrParse y
          Right y -> return x' { target = y }
      ps' <- mapM parseParameter ps
      foldM overwriteParameter x'' ps'

---

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
             
---

parseParameter
  :: String -> Either Error (String, Int)

parseParameter str =
  case parse parameterParser "Overwrite Parameter Error" str of
    Left err    -> Left $ ErrParse err
    Right (s,v) -> return (s,v)

---

parameterParser
  :: Parser (String,Int)

parameterParser = do
  name <- identifier $ makeTokenParser globalDef
  void $ char '='
  x <- many1 digit
  eof
  return (name, read x)

---

overwriteParameter
  :: Specification -> (String,Int) -> Either Error Specification

overwriteParameter s (n,v) = case find ((== n) . bIdent) $ parameters s of
  Nothing -> Left $ ErrArgs $ ArgumentsError $
            "Specification has no parameter: " ++ n 
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

---
