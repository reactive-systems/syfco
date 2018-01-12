-----------------------------------------------------------------------------
-- |
-- Module      :  Data.StringMap
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- A simple data structure to map strings to integers.
--
-----------------------------------------------------------------------------

module Data.StringMap
    ( StringMap
    , empty
    , lookup
    , insert
    ) where

-----------------------------------------------------------------------------

import Prelude hiding (lookup)

-----------------------------------------------------------------------------

-- | Internal data structure of the mapping.

data StringMap =
    Empty
  | Leaf (String, Int)
  | Node (Maybe Int, [(Char, StringMap)])

-----------------------------------------------------------------------------

-- | Returns the empty mapping.

empty
  :: StringMap

empty = Empty

-----------------------------------------------------------------------------

-- | Lookups a string in the mapping.

lookup
  :: String -> StringMap -> Maybe Int

lookup str mapping = case mapping of
  Empty       -> Nothing
  Leaf (e,v)  -> if e == str then Just v else Nothing
  Node (v,xs) -> case str of
    []   -> v
    x:xr -> case findMatch x xs of
      Just mapping' -> lookup xr mapping'
      _             -> Nothing

  where
    findMatch x xs = case xs of
      []           -> Nothing
      ((y,n) : xr) -> if x == y then Just n
                     else findMatch x xr

-----------------------------------------------------------------------------

-- | Inserts a new string-int pair to the given mapping. If the mapping
-- already containts the given string, then the corresponding value is
-- updated.

insert
  :: String -> Int -> StringMap -> StringMap

insert s i m = case m of
  Empty      -> Leaf (s,i)
  Leaf (e,v) -> if e == s then Leaf (s,i) else case e of
    []     -> Node (Just v, [(head s, Leaf (tail s,i))])
    (x:xr) -> case s of
      []     -> Node (Just i, [(x,Leaf (xr,v))])
      (y:yr) ->
        if x == y then
          Node (Nothing, [(x, insert yr i (Leaf (xr,v)))])
        else
          Node (Nothing, [(x, Leaf (xr,v)),(y, Leaf (yr,i))])
  Node (v,xs) -> case s of
    []     -> Node (Just i,xs)
    (x:xr) -> Node (v, add x xr i xs)

  where
    add x xr j xs = case xs of
      []         -> [(x, Leaf (xr,j))]
      ((c,n):yr) ->
        if x == c then
          (c,insert xr j n):yr
        else
          (c,n) : add x xr j yr

-----------------------------------------------------------------------------
