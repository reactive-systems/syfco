module Reader.Parser.Expression
       ( exprParser
       ) where

---

import Data.Expression
import Reader.Parser.Data
import Reader.Parser.Utils

import Control.Monad

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.String
import Text.Parsec.Token hiding (identifier)

---

exprParser
  :: Parser (Expr String)

exprParser = (~~) >> buildExpressionParser table term
  where
    table =
      [ [ Prefix $ unaryOperators numUnary
        ]
        
      , [ Infix  (binOp "*"        NumMul)     AssocLeft
        , Infix  (binOp "MUL"      NumMul)     AssocLeft
        ]
      , [ Infix  (binOp "/"        NumDiv)     AssocRight
        , Infix  (binOp "DIV"      NumDiv)     AssocRight
        , Infix  (binOp "%"        NumMod)     AssocRight
        , Infix  (binOp "MOD"      NumMod)     AssocRight
        ]
      , [ Infix  (binOp "+"        NumPlus)    AssocLeft
        , Infix  (binOp "PLUS"     NumPlus)    AssocLeft
        , Infix  (binOp "-"        NumMinus)   AssocLeft
        , Infix  (binOp "MINUS"    NumMinus)   AssocLeft
        ]
        
      , [ Prefix $ unaryOperators setUnary
        ]  
        
      , [ Infix  (binOp "(-)"      SetMinus)   AssocRight
        , Infix  (binOp "(\\)"     SetMinus)   AssocRight
        , Infix  (binOp "SETMINUS" SetMinus)   AssocRight
        ]
      , [ Infix  (binOp "(*)"      SetCap)     AssocLeft
        , Infix  (binOp "CAP"      SetCap)     AssocLeft
        ] 
      , [ Infix  (binOp "(+)"      SetCup)     AssocLeft
        , Infix  (binOp "CUP"      SetCup)     AssocLeft
        ]
      , [ Infix  (binOp "=="       BlnEQ)      AssocLeft
        , Infix  (binOp "EQ"       BlnEQ)      AssocLeft
        , Infix  (binOp "/="       BlnNEQ)     AssocLeft
        , Infix  (binOp "NEQ"      BlnNEQ)     AssocLeft
        , Infix  (binOp ">"        BlnGE)      AssocLeft
        , Infix  (binOp "GE"       BlnGE)      AssocLeft          
        , Infix  (binOp ">="       BlnGEQ)     AssocLeft
        , Infix  (binOp "GEQ"      BlnGEQ)     AssocLeft
        , Infix  (binOp "<"        BlnLE)      AssocLeft
        , Infix  (binOp "LE"       BlnLE)      AssocLeft          
        , Infix  (binOp "<="       BlnLEQ)     AssocLeft
        , Infix  (binOp "LEQ"      BlnLEQ)     AssocLeft
        ]
      , [ Infix  (binOp "<-"       BlnElem)    AssocLeft
        , Infix  (binOp "IN"       BlnElem)    AssocLeft          
        , Infix  (binOp "ELEM"     BlnElem)    AssocLeft          
        ]

      , [ Prefix $ unaryOperators ltlUnary
        ]  

      , [ Infix  (binOp "&&"       BlnAnd)     AssocLeft
        , Infix  (binOp "AND"      BlnAnd)     AssocLeft
        ]
      , [ Infix  (binOp "||"       BlnOr)      AssocLeft
        , Infix  (binOp "OR"       BlnOr)      AssocLeft
        ]
      , [ Infix  (binOp "->"       BlnImpl)    AssocRight
        , Infix  (binOp "IMPIES"   BlnImpl)    AssocRight
        , Infix  (binOp "<->"      BlnEquiv)   AssocRight
        , Infix  (binOp "EQUIV"    BlnEquiv)   AssocRight
        ]
      , [ Infix  (binOp "W"        LtlWeak)    AssocRight
        ]        
      , [ Infix  (binOp "U"        LtlUntil)   AssocRight
        ]
      , [ Infix  (binOp "R"        LtlRelease) AssocLeft
        ]
      , [ Infix  (binOp "~"        Pattern)    AssocLeft        
        ]        
      , [ Infix  (binOp ":"        Colon)      AssocLeft
        ]
      ]

    tokenDef =
      globalDef
      { opStart = oneOf "!&|-<=/+*%(:~,."
      , opLetter = oneOf "!&|<->=/\\[+*%():~,."
      , reservedOpNames =
          ["!","&&","||","->","<->","==","/=","<",">","<=",">=",
           "<-","&&[","||[","NOT","AND","OR","IMPLIES","EQUIV","EQ",
           "NEQ", "LE", "GE", "LEQ", "GEQ", "ELEM","AND[","OR[",
           "+","-","*","/","%","PLUS","MINUS","MUL","DIV","MOD",
           "SIZE","MIN","MAX","(-)","(\\)","(+)","(*)","SETMINUS",
           "CAP","CUP",":","~","W","U","R","X","G","F",",","X[",
           "G[","F[","AND[","OR[","SUM","PROD","IN","SIZEOF"] 
      , reservedNames =
          ["NOT","AND","OR","IMPLIES","EQUIV","true","false","F",
           "PLUS","MINUS","MUL","DIV","MOD","SIZE","MIN","MAX","_",
           "SETMINUS","CAP","CUP","otherwise","W","U","R","X","G",
           "SUM","PROD","IN","SIZEOF"] }

    tokenparser = makeTokenParser tokenDef

    term =
          parentheses
      <|> setExplicit          
      <|> between' '|' '|' (liftM NumSSize exprParser)
      <|> keyword "otherwise" BaseOtherwise
      <|> keyword "false" BaseFalse
      <|> keyword "true" BaseTrue
      <|> keyword "_" BaseWild
      <|> constant
      <|> ident

    numUnary =
          unOp6 'S' 'I' 'Z' 'E' 'O' 'F' NumSizeOf
      <|> unOp4 'S' 'I' 'Z' 'E' NumSSize
      <|> unOp3 'M' 'I' 'N' NumSMin
      <|> unOp3 'M' 'A' 'X' NumSMax
      <|> parOp "+" manyExprParser NumRPlus
      <|> parOp "SUM" manyExprParser NumRPlus      
      <|> parOp "*" manyExprParser NumRMul
      <|> parOp "PROD" manyExprParser NumRMul       

    setUnary =
          parOp "(+)" manyExprParser SetRCup
      <|> parOp "CUP" manyExprParser SetRCap              
      <|> parOp "(-)" manyExprParser SetRCup
      <|> parOp "CAP" manyExprParser SetRCap          

    ltlUnary =
          unOp' '!' BlnNot
      <|> unOp3 'N' 'O' 'T' BlnNot
      <|> unOp1 'X' LtlNext
      <|> unOp1 'G' LtlGlobally
      <|> unOp1 'F' LtlFinally
      <|> parOp "X" exprParser LtlRNext
      <|> parOp "G" exprParser LtlRGlobally
      <|> parOp "F" exprParser LtlRFinally
      <|> parOp "&&" manyExprParser BlnRAnd
      <|> parOp "AND" manyExprParser BlnRAnd
      <|> parOp "||" manyExprParser BlnROr
      <|> parOp "OR" manyExprParser BlnROr      

    parentheses = do
      notFollowedBy $ ch '(' >> oneOf "+-*/"
      between' '(' ')' $ liftM expr exprParser

    keyword x c = do
      s <- getPos
      void $ reserved tokenparser x
      return $ Expr c $ ExprPos s $
        SrcPos (srcLine s) (srcLine s + length x)

    setExplicit = do
      s <- getPos; ch '{'; (~~)
      emptySet s <|> nonEmptySet s

    emptySet s = do
      e <- closeSet
      return $ Expr (SetExplicit []) (ExprPos s e) 

    nonEmptySet s = do
      x <- exprParser
      singeltonSet s x <|> nonSingeltonSet s x

    singeltonSet s x = do
      e <- closeSet
      return $ Expr (SetExplicit [x]) (ExprPos s e)

    nonSingeltonSet s x = do
      ch ','; (~~)
      y <- exprParser
      twoElmSet s x y <|> rangeSet s x y <|> manyElmSet s x y

    twoElmSet s x y = do
      e <- closeSet
      return $ Expr (SetExplicit [x,y]) (ExprPos s e)

    rangeSet s x y = do
      ch '.'; ch '.'; (~~)
      z <- exprParser
      e <- closeSet
      return $ Expr (SetRange x y z) (ExprPos s e)

    manyElmSet s x y = do
      ch ','; (~~)
      xs <- manyExprParser
      e <- closeSet
      return $ Expr (SetExplicit (x:y:xs)) (ExprPos s e)

    closeSet = do { ch '}'; e <- getPos; (~~); return e }

    binOp x c = do
      reservedOp tokenparser x
      return $ \a b -> Expr (c a b) $ 
                       ExprPos (srcBegin $ srcPos a) $
                       srcEnd $ srcPos b

    unaryOperators p = do
      (x:xr) <- many1 $ unaryOperator p
      return $ conUnOp x xr

    unaryOperator p = do
      s <- getPos
      c <- p
      return (s,c)

    conUnOp (s,c) xs = case xs of
      []     -> \e -> Expr (c e) $
                      ExprPos s $ srcEnd $ srcPos e
      (x:xr) -> \e -> Expr (c $ conUnOp x xr e) $
                      ExprPos s $ srcEnd $ srcPos e

    unOp6 c1 c2 c3 c4 c5 c6 c = try $ do
      ch c1; ch c2; ch c3; ch c4; ch c5; ch c6
      lookAhead $ (ch ' ' <|> ch '(' <|> ch '\t' <|> ch '\n')
      (~~)
      return c
      
    unOp4 c1 c2 c3 c4 c = try $ do
      ch c1; ch c2; ch c3; ch c4; 
      lookAhead $ (ch ' ' <|> ch '(' <|> ch '\t' <|> ch '\n')
      (~~)
      return c

    unOp' x c = do
      ch x; (~~)
      return c      

    unOp1 x c = try $ do
      ch x; 
      lookAhead $ (ch ' ' <|> ch '(' <|> ch '\t' <|> ch '\n')
      (~~)      
      return c

    unOp3 c1 c2 c3 c = try $ do
      ch c1; ch c2; ch c3;
      lookAhead $ (ch ' ' <|> ch '(' <|> ch '\t' <|> ch '\n')
      (~~)      
      return c        

    parOp x p c = do
      reservedOp tokenparser (x ++ "[")
      e <- p; ch ']'; (~~)
      return (c e)            
      
    between' c1 c2 p = do
      s <- getPos; ch c1; (~~); x <- p
      ch c2; e <- getPos; (~~)
      return $ Expr x $ ExprPos s e

    constant = do
      (x,pos) <- positionParser (~~) $ many1 digit
      return $ Expr (BaseCon $ read x) pos

    ident = do
      (i,pos) <- identifier (~~)
      functionParser pos i 
        <|> busParser pos i 
        <|> return (Expr (BaseId i) pos)

    functionParser pos i = do
      notFollowedBy $ ch '(' >> oneOf "+-*/"
      ch '('; (~~)
      ys <- manyExprParser
      ch ')'; e <- getPos; (~~)
      return $ Expr (BaseFml ys i) $
        ExprPos (srcBegin pos) e

    busParser pos i = do
      ch '['; (~~)
      x <- exprParser
      ch ']'; p <- getPos; (~~)
      return $ Expr (BaseBus x i) $
        ExprPos (srcBegin pos) p
        
    manyExprParser = commaSep tokenparser exprParser          

    (~~) = whiteSpace tokenparser
    
    ch = void . char        

---

        

  
     
