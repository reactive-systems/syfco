-----------------------------------------------------------------------------
-- |
-- Module      :  Writer.Error
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
-- 
-- Pretty and informative error messages that may be thrown while writing 
-- out a specification to another format.
-- 
-----------------------------------------------------------------------------

module Writer.Error
    ( Error
    , errBounds
    , errBusCmp  
    , errMinSet
    , errMaxSet
    , errSetCap
    , errNoMatch
    , errToLower
    , errNoGR1  
    , prError      
    ) where

-----------------------------------------------------------------------------
    
import Data.Error
    ( Error
    , runtimeError
    , conversionError
    , prError
    )  
    
import Data.Expression
    ( ExprPos
    )
    
import Control.Monad.State
    ( StateT(..)
    )  

-----------------------------------------------------------------------------

-- | Throws an error that indicates an out of bounds access to a bus.

errBounds
  :: String -> Int -> Int -> ExprPos -> StateT a (Either Error) ()

errBounds i u r pos =
  let msg = "index out of bounds: " ++ i ++ "[" ++ show r ++ "]" ++ "\n" ++
            "valid range:         0 - " ++ show (u - 1)  
  in StateT $ \_ -> runtimeError pos msg

-----------------------------------------------------------------------------

-- | Throws an error that indicates a comparison of two busses of
-- unequal length.

errBusCmp
  :: String -> String -> String -> Int -> Int -> ExprPos -> StateT a (Either Error) b

errBusCmp v1 v2 cm i j pos =
  let msg = "invalid comparison: " ++ v1 ++ " " ++ show cm ++ " " ++ v2
            ++ "\n" ++
            "they are of unequal length: " ++ show i ++ " != " ++ show j 
  in StateT $ \_ -> runtimeError pos msg                    

-----------------------------------------------------------------------------

-- | Throws an error that indicates that the minimum operation is applied
-- to an empty set.

errMinSet
  :: ExprPos -> StateT a (Either Error) b

errMinSet pos = 
  let msg = "Cannot compute the minumum of an empty set."
  in StateT $ \_ -> runtimeError pos msg

-----------------------------------------------------------------------------

-- | Throws an error that indicates that the maximum operation is applied
-- to an empty set.

errMaxSet
  :: ExprPos -> StateT a (Either Error) b

errMaxSet pos = 
  let msg = "Cannot compute the maximum of an empty set."
  in StateT $ \_ -> runtimeError pos msg

-----------------------------------------------------------------------------

-- | Throws an error that indicates that the intersection operation is
-- to an empty list of sets.

errSetCap
  :: ExprPos -> StateT a (Either Error) b

errSetCap pos = 
  let msg = "Cannot compute the intersection of an empty set of sets."
  in StateT $ \_ -> runtimeError pos msg

-----------------------------------------------------------------------------

-- | Throws an error that indicates that during the evaluation of a function
-- no guard could be applied.

errNoMatch
  :: String -> [String] -> ExprPos -> StateT a (Either Error) b

errNoMatch i xs pos = 
  let msg = "there is no positive guard to evaluate:\n" ++
            "  " ++ i ++ "(" ++
            (if null xs then "" else
               head xs ++ concatMap ((:) ',' . (:) ' ') (tail xs)) ++
            ")"
  in StateT $ \_ -> runtimeError pos msg

-----------------------------------------------------------------------------

-- | Throws an error that indicates a pair of variables that would clash
-- if converted to lower case.

errToLower
  :: String -> String -> String -> ExprPos -> Either Error a 

errToLower fmt n1 n2 pos =
  let msg = "The " ++ fmt ++ " format only accepts lower case variables. " ++
            "However, automatic conversion introduces a clash, since " ++
            "then '" ++ n1 ++ "' equals '" ++ n2 ++ "'."
  in runtimeError pos msg

-----------------------------------------------------------------------------

-- | Throws an error that indicates that the given formula is not in GR1,
-- which is neccessary for the corresponding conversation.

errNoGR1
  :: String -> String -> Either Error a 

errNoGR1 t fmt =
  let msg = "The given specification is not in GR(1), which is " ++
            "neccessary to convert to the " ++ fmt ++ " format."
  in conversionError t msg

-----------------------------------------------------------------------------
