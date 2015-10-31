-----------------------------------------------------------------------------
-- |
-- Module      :  Reader.Parser.Info
-- Description :  Parser for the INFO section
-- License     :  MIT (see the LICENSE file)
-- 
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
-- 
-- Parser for the INFO section
-- 
-----------------------------------------------------------------------------

module Reader.Parser.Info
    ( infoParser
    , targetParser
    , semanticsParser
    ) where

-----------------------------------------------------------------------------

import Data.Types
    ( Semantics(..)
    , Target(..)
    )  

import Reader.Parser.Data
    ( globalDef
    )
    
import Reader.Parser.Utils
    ( stringParser
    , identifier
    )  

import Control.Monad
    ( void
    )  

import Data.Functor.Identity
    ( Identity
    )  

import Text.Parsec
    ( ParsecT
    , (<|>)
    , char
    , unexpected  
    , parserFail  
    )
    
import Text.Parsec.String
    ( Parser
    )
    
import Text.Parsec.Token
    ( TokenParser
    , commaSep
    , reservedNames  
    , whiteSpace
    , makeTokenParser
    , reserved  
    )  

-----------------------------------------------------------------------------

-- | Parses the INFO section of a specification file. It returns:
-- 
--     * the title of the specification
-- 
--     * the description of the specification
-- 
--     * the semantics of the specification
-- 
--     * the target of the specification
-- 
--     * the tag list of the specification

infoParser
  :: Parser (String, String, Semantics, Target, [String])

infoParser = (~~) >> do
  keyword "INFO"
  ch '{'
  infoContentParser Nothing Nothing Nothing Nothing Nothing

  where    
    infoContentParser t d y g a =
          do { keyword "TITLE"; titleParser t d y g a }
      <|> do { keyword "DESCRIPTION"; descriptionParser t d y g a}
      <|> do { keyword "SEMANTICS"; semanticsParser' t d y g a }
      <|> do { keyword "TARGET"; targetParser' t d y g a }
      <|> do { keyword "TAGS"; tagsParser t d y g a }          
      <|> do { ch '}'; endParser  t d y g a }
      
    titleParser t d y g a = case t of
      Nothing -> ch ':' >> do
        str <- stringParser; (~~)
        infoContentParser (Just str) d y g a
      _       -> errDoubleDef "TITLE"

    descriptionParser t d y g a = case d of
      Nothing -> ch ':' >> do
        str <- stringParser; (~~)
        infoContentParser t (Just str) y g a
      _       -> errDoubleDef "DESCRIPTION"

    semanticsParser' t d y g a = case y of
      Nothing -> ch ':' >> do
        v <- semanticsParser
        infoContentParser t d (Just v) g a
      _       -> errDoubleDef "TYPE"

    targetParser' t d y g a = case g of
      Nothing -> ch ':' >> do
        x <- targetParser 
        infoContentParser t d y (Just x) a
      _       -> errDoubleDef "TARGET"

    tagsParser t d y g a = case a of
      Nothing -> ch ':' >> do
        xs <- commaSep tokenparser (identifier (~~))
        infoContentParser t d y g (Just $ map fst xs)
      _       -> errDoubleDef "TAGS"

    endParser t d y g a = case (t,d,y,g) of
      (Nothing, _, _, _)               -> errMissing "TITLE"
      (_, Nothing, _, _)               -> errMissing "DESCRIPTION"
      (_, _, Nothing, _)               -> errMissing "SEMANTICS"
      (_, _, _, Nothing)               -> errMissing "TARGET"
      (Just u, Just v, Just w, Just x) -> case a of
        Just ts -> return (u,v,w,x,ts)
        _       -> return (u,v,w,x,[]) 


    ch x = void $ char x >> (~~)
    (~~) = whiteSpace tokenparser
    
    errMissing str =
      parserFail $
      "The " ++ str ++ " entry is missing in the INFO section."
    errDoubleDef str =
      unexpected $
      str ++ " (already defined)"

-----------------------------------------------------------------------------

-- | Parses the target description.

targetParser
  :: Parser Target

targetParser = 
      do { keyword "Mealy"; return TargetMealy }
  <|> do { keyword "Moore"; return TargetMoore }

-----------------------------------------------------------------------------      

-- | Parses the semantics description.

semanticsParser
  :: Parser Semantics

semanticsParser = do
  x <- semanticKeyword
  z <- do { void $ char ','; semanticKeyword } <|> (return "none")
  case (x,z) of
    ("mealy","none")  -> return SemanticsMealy
    ("moore","none")  -> return SemanticsMoore
    ("mealy","strict") -> return SemanticsStrictMealy
    ("strict","mealy") -> return SemanticsStrictMealy
    ("moore","strict") -> return SemanticsStrictMoore
    ("strict","moore") -> return SemanticsStrictMoore
    ("mealy","moore")  -> unexpected "Moore"
    ("moore","moore")  -> unexpected "Mealy"
    ("moore","mealy")  -> unexpected "Mealy"
    ("mealy","mealy")  -> unexpected "Mealy"
    _                  -> unexpected "Strict"

  where
    semanticKeyword =
          do { keyword "Mealy"; return "mealy" }
      <|> do { keyword "Moore"; return "moore" }
      <|> do { keyword "Strict"; return "strict" }

-----------------------------------------------------------------------------          

tokenparser
  :: TokenParser a

tokenparser =
  makeTokenParser globalDef
  { reservedNames  =
       ["INFO","TITLE","DESCRIPTION", "SEMANTICS",
        "TAGS","Strict","Mealy","Moore","TARGET"] }

-----------------------------------------------------------------------------            

keyword
  :: String -> ParsecT String u Identity ()

keyword = void . reserved tokenparser

-----------------------------------------------------------------------------            
