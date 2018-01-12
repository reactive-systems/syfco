-----------------------------------------------------------------------------
-- |
-- Module      :  Utils
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- Functions on standard data types that are not in Prelude.
--
-----------------------------------------------------------------------------

module Utils
    ( strictSort
    , bucketSort
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

import Data.Set
    ( elems
    , fromList
    )

import Data.Ix
    ( Ix
    )

import qualified Data.Array.ST as A
import Control.Monad.ST

-----------------------------------------------------------------------------

-- | Strict version of 'sort'.

strictSort
  :: Ord a => [a] -> [a]

strictSort  = elems . fromList

-----------------------------------------------------------------------------

-- | Strict version of 'sort' for indexable types using array buckets.

bucketSort
  :: Num i => Ix i => [i] -> [i]

bucketSort xs = case xs of
  []     -> []
  (x:xr) ->
    let bounds = foldl (\(a,b) y -> (min a y, max b y)) (x,x) xr
    in runST (bucketSortST bounds xs)

  where
    bucketSortST (l,u) ys =
      let
        newArray :: Ix j => (j, j) -> Int -> ST s (A.STArray s j Int)
        newArray = A.newArray
      in do
        a <- newArray (l,u) 0
        mapM_ (incIdx a) ys
        getPositive l a [] u

    incIdx a i = do
      v <- A.readArray a i
      A.writeArray a i (v+1)

    getPositive l a b i
      | i < l     = return b
      | otherwise = do
        v <- A.readArray a i
        if v > 0 then
          getPositive l a (i:b) (i-1)
        else
          getPositive l a b (i-1)

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
