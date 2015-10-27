module Data.Error where

---

import Data.Expression
import Text.Parsec.Error
import System.Exit
import System.IO

---

data Error =
    ErrType TypeError
  | ErrParse ParseError
  | ErrBnd BindingError
  | ErrDep DependencyError
  | ErrArgs ArgumentsError
  | ErrSyntax SyntaxError
  | ErrRunT RunTimeError


data TypeError =
  TypeError
  { errTPos :: ExprPos
  , errTMsgs :: [String]
  } deriving (Eq, Ord)

data BindingError =
  BindingError
  { errBPos :: ExprPos
  , errBMsgs :: [String]
  } deriving (Eq, Ord)

data DependencyError =
  DependencyError
  { errDPos :: ExprPos
  , errDMsgs :: [String]
  } deriving (Eq, Ord)

data SyntaxError =
  SyntaxError
  { errSPos :: ExprPos
  , errSMsgs :: [String]
  } deriving (Eq, Ord)

data RunTimeError =
  RunTimeError
  { errRPos :: ExprPos
  , errRMsgs :: [String]
  } deriving (Eq, Ord)                          

data ArgumentsError =
  ArgumentsError
  { message :: String
  }

---

prErrPos
  :: ExprPos -> String

prErrPos pos =
  "line " ++ show bl ++ ", column " ++ show bc ++
  if bl == el then
    " - " ++ show ec
  else
    " - line " ++ show el ++ ", column " ++ show ec

  where
    bl = srcLine $ srcBegin pos
    bc = srcColumn $ srcBegin pos
    el = srcLine $ srcEnd pos
    ec = srcColumn $ srcEnd pos

---

syntaxError
  :: ExprPos -> String -> Either Error a

syntaxError pos msg = Left $ ErrSyntax $ SyntaxError pos [msg]

---

runtimeError
  :: ExprPos -> String -> Either Error a

runtimeError pos msg = Left $ ErrRunT $ RunTimeError pos [msg]    

---    

typeError
  :: ExprPos -> String -> Either Error a

typeError pos msg = Left $ ErrType $ TypeError pos [msg]

---

bindingError
  :: ExprPos -> String -> Either Error a

bindingError pos msg = Left $ ErrBnd $ BindingError pos [msg]

---

depError
  :: ExprPos -> String -> Either Error a

depError pos msg = Left $ ErrDep $ DependencyError pos [msg]

---

argsError
  :: String -> Either Error a

argsError msg = Left $ ErrArgs $ ArgumentsError msg

---

prError
  :: Error -> IO a

prError err = do
  hPutStrLn stderr $ case err of
    ErrType x   ->
      "\"Type Error\" (" ++ prErrPos (errTPos x) ++ "):\n"++ concat (errTMsgs x)      
    ErrBnd x    ->
      "\"Binding Error\" (" ++ prErrPos (errBPos x) ++ "):\n"++ concat (errBMsgs x)      
    ErrDep x    ->
      "\"Dependency Error\" (" ++ prErrPos (errDPos x) ++ "):\n"++ concat (errDMsgs x)      
    ErrArgs x   ->
      "\"Error\" " ++ message x
    ErrSyntax x ->
      "\"Syntax Error\" (" ++ prErrPos (errSPos x) ++ "):\n"++ concat (errSMsgs x)
    ErrRunT x ->
      "\"Runtime Error\" (" ++ prErrPos (errRPos x) ++ "):\n"++ concat (errRMsgs x)      
    ErrParse x  ->
      show x
      
  exitFailure
  
---
