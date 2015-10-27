module Reader.Parser.Component
       ( componentParser
       ) where

---

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token hiding (identifier)

import Data.Binding
import Data.Expression
import Reader.Parser.Data (globalDef)
import Reader.Parser.Utils
import Reader.Parser.Expression

import Data.Maybe 
import Control.Monad 

---

data Component =
  Component
  { inputs :: [Bind Expr String]
  , outputs :: [Bind Expr String]
  , assumptions :: [Expr String]
  , invariants :: [Expr String]
  , guarantees :: [Expr String]
  }

---  

componentParser
  :: Parser ([Bind Expr String], [Bind Expr String], [Expr String], [Expr String], [Expr String])

componentParser = do
  keyword "MAIN"
  xs <- br $ many $ componentContentParser 
        Component
        { inputs = []                   
        , outputs = []
        , assumptions = []
        , invariants = []
        , guarantees = []
        }
        
  return
    ( concatMap inputs xs
    , concatMap outputs xs
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
          , "ASSUMPTIONS"
          , "INVARIANTS"
          , "GUARANTEES"
          ]
      }

    componentContentParser c =
          (sectionParser "INPUTS"      signalParser >>= \x -> return c { inputs = x      })      
      <|> (sectionParser "OUTPUTS"     signalParser >>= \x -> return c { outputs = x     })
      <|> (sectionParser "ASSUMPTIONS" exprParser   >>= \x -> return c { assumptions = x })
      <|> (sectionParser "INVARIANTS"  exprParser   >>= \x -> return c { invariants = x  })
      <|> (sectionParser "GUARANTEES"  exprParser   >>= \x -> return c { guarantees = x  })

    signalParser = do
      (x,pos) <- identifier (~~)
      busParser x pos <|> return (Bind x [] pos [])

    busParser x pos = do
      ch '['; (~~)
      e <- exprParser
      ch ']'; p <- getPos; (~~)
      return $ Bind x [] (ExprPos (srcBegin pos) p) [e]

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
    
---
