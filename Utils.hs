-----------------------------------------------------------------------------
-- |
-- Module      :  Utils
-- Description :  Functions on standard data types that are not in Prelude
-- License     :  MIT (see the LICENSE file)
-- 
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
-- 
-- Functions on standard data types that are not in Prelude
-- 
-----------------------------------------------------------------------------

module Utils
    ( strictsort
    , iter
    , imLookup
    ) where

-----------------------------------------------------------------------------

import qualified Data.IntMap as IM
    ( Key
    , IntMap
    , lookup
    )

import Data.Maybe
    ( fromJust
    )

-----------------------------------------------------------------------------

-- | Strict version of 'sort'.

strictsort
  :: Ord a => [a] -> [a]
     
strictsort vs = case vs of
  []  -> []
  [x] -> [x]
  _   -> let (ys,zs) = split [] [] vs
        in  strictmerge [] (strictsort ys) (strictsort zs)

  where
    strictmerge a xs ys = case (xs,ys) of
      (_,[])      -> rappend a xs
      ([],_)      -> rappend a ys
      (x:xr,y:yr) -> case compare x y of
        LT -> strictmerge (x:a) xr (y:yr)
        EQ -> strictmerge (x:a) xr yr
        GT -> strictmerge (y:a) (x:xr) yr
        
    rappend xs ys = foldl (flip (:)) ys xs

    split a b xs = case xs of
      (x:xr) -> split b (x:a) xr      
      []     -> (a,b)

-----------------------------------------------------------------------------

-- | @iter f n s@ applies the function @f@ @n@ times to @s@.

iter
  :: (a -> a) -> Int -> a -> a

iter f n s =
  if n == 0 then s
  else iter f (n-1) (f s)

-----------------------------------------------------------------------------

-- | @imLookup n im@ looks up an element @n@ of an int map @im@ and takes it
-- out of the 'Maybe' monad. The function assumes that the corresponding
-- element exists in @im@.

imLookup
  :: IM.Key -> IM.IntMap a -> a

imLookup n im =
  fromJust $ IM.lookup n im

-----------------------------------------------------------------------------
