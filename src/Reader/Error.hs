-----------------------------------------------------------------------------
-- |
-- Module      :  Reader.Error
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
-- 
-- Pretty and informative error messages that may be thrown while reading
-- the specification.
-- 
-----------------------------------------------------------------------------

module Reader.Error
    ( Error
    , errUnknown
    , errConflict
    , errPattern
    , errArgArity
    , errConditional
    , errCircularDep
    , errExpect
    , errRange
    ) where

-----------------------------------------------------------------------------

import Data.Types
    ( IdType
    )  

import Data.Error
    ( Error
    , depError
    , typeError
    , syntaxError
    , bindingError  
    , prErrPos
    )
    
import Data.Expression
    ( ExprPos
    )
    
import Control.Monad.State
    ( StateT(..)
    )  

-----------------------------------------------------------------------------

-- | Throws an error that indicates an unbound identifier name.

errUnknown
  :: String -> ExprPos -> StateT a (Either Error) b

errUnknown i pos =
  let msg = "identifiyer not in scope: " ++ i
  in StateT $ \_ -> bindingError pos msg
     
-----------------------------------------------------------------------------

-- | Throws an error that indicates two conflicting identifier bindings.

errConflict
  :: String -> ExprPos -> ExprPos -> StateT a (Either Error) b

errConflict i x y = 
  let msg = "conflicting definitions: " ++ i ++ "\n" ++
            "already bound at " ++ prErrPos x
  in StateT $ \_ -> bindingError y msg

-----------------------------------------------------------------------------

-- | Throws an error informing the user that formulas cannot be used
-- as a right hand side of a pattern matching.

errPattern
  :: ExprPos -> StateT a (Either Error) b

errPattern pos =
  let msg = "Formulas are not allowed on the right hand side " ++
            "of a pattern match."
  in StateT $ \_ -> typeError pos msg

-----------------------------------------------------------------------------

-- | Throws an error that inidactes two many arguments passed to a
-- function.

errArgArity
  :: String -> Int -> ExprPos -> ExprPos -> StateT a (Either Error) b

errArgArity i n x y =
  let msg = "unexpected number of arguments: " ++ i ++ "\n" ++
            "According to its definition (" ++  prErrPos x ++
            "),\nthe function has to be applied to " ++ show n ++
            " arguments."
  in StateT $ \_ -> typeError y msg

-----------------------------------------------------------------------------

-- | Throws an error that indicates a sub-expression that does not conform
-- to the big-operator notation.

errConditional 
  :: ExprPos -> StateT a (Either Error) b

errConditional pos =
  let msg = "expecting expression of the form:\n" ++
            "  identifyer <- set"
  in StateT $ \_ -> syntaxError pos msg

-----------------------------------------------------------------------------

-- | Throws an error that indicates a set of identifiers that decribe a 
-- circular dependency between each other.

errCircularDep
  :: [(String,ExprPos)] -> ExprPos -> Either Error b

errCircularDep xs pos =
  let
    m = foldl max (length $ fst $ head xs) $ map (length . fst) xs
    msg = "detected circular dependencies between:" ++
            (concatMap
             (\(x,y) -> "\n  " ++ x
                        ++ (replicate (m - length x) ' ') ++
                        " (defined at " ++ prErrPos y ++ ")") xs) ++
            if (length xs > 1) then "" else " depends on itself"
  in depError pos msg

-----------------------------------------------------------------------------

-- | Throws an error that incicates a wrongly typed subexpression.

errExpect
  :: IdType -> IdType -> ExprPos -> StateT a (Either Error) b

errExpect x y pos =
  let msg = "expecting: " ++ show x ++ " expression\n" ++
            "but found: " ++ show y ++ " expression"
  in StateT $ \_ -> typeError pos msg

-----------------------------------------------------------------------------

-- | Throws an error that incicates a sub-expression that does not conform
-- to the range syntax.

errRange
  :: IdType -> ExprPos -> StateT a (Either Error) b

errRange x pos =
  let msg = "expecting: range expression\n" ++
            "but found: " ++ show x ++ " expression"
  in StateT $ \_ -> typeError pos msg

-----------------------------------------------------------------------------
