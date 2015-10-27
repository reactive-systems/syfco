module Data.StringMap
       ( StringMap
       , empty
       , lookup
       , insert
       ) where

---

import Prelude hiding (lookup)

---

data StringMap = E | L (String,Int) | N (Maybe Int,[(Char, StringMap)]) deriving (Show)

---

empty
  :: StringMap

empty = E

---

lookup
  :: String -> StringMap -> Maybe Int

lookup s m = case m of
  E        -> Nothing
  L (e,v)  -> if (e == s) then Just v else Nothing
  N (v,xs) -> case s of
    []   -> v
    x:xr -> case find' x xs of
      Just m' -> lookup xr m'
      _       -> Nothing

  where
    find' x xs = case xs of
      ((y,n) : xr) ->
        if x == y then Just n else find' x xr      
      _ -> Nothing

---

insert
  :: String -> Int -> StringMap -> StringMap

insert s i m = case m of
  E       -> L (s,i)
  L (e,v) -> if e == s then L (s,i) else case e of
    []     -> N (Just v, [(head s, L (tail s,i))])
    (x:xr) -> case s of
      []     -> N (Just i, [(x,L (xr,v))])
      (y:yr) ->
        if x == y then
          N (Nothing, [(x, insert yr i (L (xr,v)))])
        else
          N (Nothing, [(x, L (xr,v)),(y, L (yr,i))])
  N (v,xs) -> case s of
    []     -> N (Just i,xs)
    (x:xr) -> N (v, add x xr i xs)

  where
    add x xr j xs = case xs of
      []         -> [(x, L (xr,j))]
      ((c,n):yr) ->
        if x == c then
          ((c,insert xr j n):yr)
        else
          ((c,n) : add x xr j yr)

---
