module Utils where

---

import qualified Data.IntMap as IM (Key, IntMap, lookup)
import Data.Maybe (fromJust)

---

split
  :: [a] -> ([a],[a])

split = split' [] []
  where
    split' a b [] = (a,b)
    split' a b (x:xr) = split' b (x:a) xr

---

strictsort
  :: Ord a => [a] -> [a]
     
strictsort vs = case vs of
  []  -> []
  [x] -> [x]
  _   -> let (ys,zs) = split vs
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

---

iter
  :: (a -> a) -> Int -> a -> a

iter f i a = if i == 0 then a else iter f (i-1) (f a)
    
---

map2
  :: (a -> b) -> (c -> d) -> [(a,c)] -> [(b,d)]

map2 f1 f2 = map (\(x,y) -> (f1 x, f2 y))

---

imLookup
  :: IM.Key -> IM.IntMap a -> a

imLookup i t =
  fromJust $ IM.lookup i t

--  
