module Reader.Parser.Info
       ( infoParser
       , targetParser
       , semanticsParser
       ) where

---

import Data.Types
import Reader.Parser.Data
import Reader.Parser.Utils

import Control.Monad
import Data.Functor.Identity

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token hiding (identifier)

---

data Sem = SE | SO | SS | SN

---

infoParser
  :: Parser (String,String,Semantics,Target,[String])

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

---

targetParser
  :: Parser Target

targetParser = 
      do { keyword "Mealy"; return TargetMealy }
  <|> do { keyword "Moore"; return TargetMoore }

---      

semanticsParser
  :: Parser Semantics

semanticsParser = do
  x <-     do { keyword "Mealy"; return SE }
      <|> do { keyword "Moore"; return SO }
      <|> do { keyword "Strict"; return SS }
  z <-     do { void $ char ',';
                   do { keyword "Mealy"; return SE }
               <|> do { keyword "Moore"; return SO }
               <|> do { keyword "Strict"; return SS } }
      <|> (return SN)
  case (x,z) of
    (SE,SN) -> return SemanticsMealy
    (SO,SN) -> return SemanticsMoore
    (SE,SS) -> return SemanticsStrictMealy
    (SS,SE) -> return SemanticsStrictMealy
    (SO,SS) -> return SemanticsStrictMoore
    (SS,SO) -> return SemanticsStrictMoore
    (SE,SO) -> unexpected "Moore"
    (SO,SO) -> unexpected "Mealy"
    (SO,SE) -> unexpected "Mealy"
    (SE,SE) -> unexpected "Mealy"
    _       -> unexpected "Strict"                     

---

tokenparser
  :: TokenParser a

tokenparser =
  makeTokenParser globalDef
  { reservedNames  =
       ["INFO","TITLE","DESCRIPTION", "SEMANTICS",
        "TAGS","Strict","Mealy","Moore","TARGET"] }

---    

keyword
  :: String -> ParsecT String u Identity ()

keyword = void . reserved tokenparser

---
