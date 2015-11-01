module Writer.Error where

---

import Data.Types
import Data.Error
import Data.Expression
import Control.Monad.State

---

errBounds
  :: String -> Int -> Int -> ExprPos -> StateT a (Either Error) ()

errBounds i u r pos =
  let msg = "index out of bounds: " ++ i ++ "[" ++ show r ++ "]" ++ "\n" ++
            "valid range:         0 - " ++ show (u - 1)  
  in StateT $ \_ -> runtimeError pos msg
     
---

errMinSet
  :: ExprPos -> StateT a (Either Error) b

errMinSet pos = 
  let msg = "Cannot compute the minumum of an empty set."
  in StateT $ \_ -> runtimeError pos msg

---

errMaxSet
  :: ExprPos -> StateT a (Either Error) b

errMaxSet pos = 
  let msg = "Cannot compute the maximum of an empty set."
  in StateT $ \_ -> runtimeError pos msg

---

errSetCap
  :: ExprPos -> StateT a (Either Error) b

errSetCap pos = 
  let msg = "Cannot compute the intersection of an empty set of sets."
  in StateT $ \_ -> runtimeError pos msg

---                    

errNoMatch
  :: String -> [String] -> ExprPos -> StateT a (Either Error) b

errNoMatch i xs pos = 
  let msg = "there is no positive guard to evaluate:\n" ++
            "  " ++ i ++ "(" ++
            (if null xs then "" else
               head xs ++ (concatMap ((:) ',' . (:) ' ') $ tail xs)) ++
            ")"
  in StateT $ \_ -> runtimeError pos msg

---

errCircularDep
  :: [(String,ExprPos)] -> ExprPos -> Either Error b

errCircularDep xs pos =
  let
    m = foldl max (length $ fst $ head xs) $ map (length . fst) xs
    msg = "detected circular dependencies between:" ++
            (concatMap (\(x,y) -> "\n  " ++ x ++ (replicate (m - length x) ' ') ++
                                  "  (defined at " ++ prErrPos y ++ ")") xs)
  in depError pos msg

---

errExpect
  :: IdType -> IdType -> ExprPos -> StateT a (Either Error) b

errExpect x y pos =
  let msg = "expecting: " ++ show x ++ " expression\n" ++
            "but found: " ++ show y ++ " expression"
  in StateT $ \_ -> typeError pos msg

---

errRange
  :: IdType -> ExprPos -> StateT a (Either Error) b

errRange x pos =
  let msg = "expecting: range expression\n" ++
            "but found: " ++ show x ++ " expression"
  in StateT $ \_ -> typeError pos msg

---


