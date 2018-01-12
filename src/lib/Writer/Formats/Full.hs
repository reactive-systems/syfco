-----------------------------------------------------------------------------
-- |
-- Module      :  Writer.Formats.Full
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- Returns a specification in full TLSF.
--
-----------------------------------------------------------------------------

module Writer.Formats.Full where

-----------------------------------------------------------------------------

import Config

import Data.Error
import Data.Binding
import Data.Expression
import Data.SymbolTable
import Data.Specification

import Data.List
import Data.Array as A

-----------------------------------------------------------------------------

-- | Replaces a list of positions in a given string.

replaces
  :: [(ExprPos, String)] -> String -> String

replaces xs str =
  let
    -- create an index mapping for the string
    idx = indexer str
    -- convert all entries in the list
    ys = sort $ map (\(p,s) -> (idx $ srcBegin p, idx $ srcEnd p, s)) xs
    -- recursively replace the entries
    (_,_,_,zs) = foldl rep (1, 0, ys, []) str
  in
    reverse zs

  where
    rep a x = case a of
      (i, v, [], xr)
        | i < v     -> (i+1, v, [], xr)
        | otherwise -> (i+1, v, [], x:xr)
      (i, v, (s,e,z):yr, xr)
        | i < v     -> (i+1, v, (s,e,z):yr, xr)
        | i < s     -> (i+1, v, (s,e,z):yr, x:xr)
        | otherwise -> (i+1, e, yr, reverse z ++ xr)

    indexer s =
      let
        -- split string into lines
        ls = map length $ lines s
        -- annotate each line with it's line number
        ys = zip [1,2..length ls] ls
        -- annotate each line with the index in the string
        (_,zs) = foldl (\(n,rs) (i,m) -> (n+m+1, (i,n):rs)) (0,[]) ys
        -- create a mapping from lines to the index of the start
        a = A.array (1,length ls) $ reverse zs
      in
       -- return a mapping that maps a position to the index
       \pos -> a ! srcLine pos + srcColumn pos

-----------------------------------------------------------------------------

-- | Full TLSF writer.

writeFormat
  :: Configuration -> Specification -> Either Error String

writeFormat c s = do
  xs <- mapM parToRep $ owParameter c
  return $ replaces xs $ source s

  where
    parToRep (str,v) =
      case findParam str of
        Nothing -> cfgError $ "Specification has no parameter: " ++ str
        Just p  -> return (p, show v)

    findParam str =
      let f x = str == idName (symboltable s ! bIdent x)
      in case filter f $ parameters s of
        []  -> Nothing
        x:_ -> return $ srcPos $ head $ bVal x

-----------------------------------------------------------------------------
