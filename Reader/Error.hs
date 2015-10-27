module Reader.Error where

---

import Data.Types
import Data.Error
import Data.Expression
import Control.Monad.State

---

errUnknown
  :: String -> ExprPos -> StateT a (Either Error) b

errUnknown i pos =
  let msg = "identifiyer not in scope: " ++ i
  in StateT $ \_ -> bindingError pos msg
     
---

errConflict
  :: String -> ExprPos -> ExprPos -> StateT a (Either Error) b

errConflict i x y = 
  let msg = "conflicting definitions: " ++ i ++ "\n" ++
            "already bound at " ++ prErrPos x
  in StateT $ \_ -> bindingError y msg

---

errPattern
  :: ExprPos -> StateT a (Either Error) b

errPattern pos =
  let msg = "Formulas are not allowed on the right hand side of a pattern match."
  in StateT $ \_ -> typeError pos msg

---

errArgArity
  :: String -> Int -> ExprPos -> ExprPos -> StateT a (Either Error) b

errArgArity i n x y =
  let msg = "unexpected number of arguments: " ++ i ++ "\n" ++
            "According to its definition (" ++  prErrPos x ++
            "),\nthe function has to be applied to " ++ show n ++ " arguments."
  in StateT $ \_ -> typeError y msg

---                    

errConditional 
  :: ExprPos -> StateT a (Either Error) b

errConditional pos =
  let msg = "expecting expression of the form:\n" ++
            "  identifyer <- set"
  in StateT $ \_ -> syntaxError pos msg

---

errCircularDep
  :: [(String,ExprPos)] -> ExprPos -> Either Error b

errCircularDep xs pos =
  let
    m = foldl max (length $ fst $ head xs) $ map (length . fst) xs
    msg = "detected circular dependencies between:" ++
            (concatMap (\(x,y) -> "\n  " ++ x ++ (replicate (m - length x) ' ') ++
                                  " (defined at " ++ prErrPos y ++ ")") xs) ++
            if (length xs > 1) then "" else " depends on itself"
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


