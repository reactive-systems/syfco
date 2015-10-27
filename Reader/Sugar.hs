module Reader.Sugar where

---

import Data.Error
import Data.Binding
import Reader.Data
import Data.Expression

import Data.Either

---

replaceSugar
  :: Specification -> Either Error Specification

replaceSugar s = do
  vs <- mapM rpsBinding $ definitions s
  return s { definitions = vs }

---

rpsBinding
  :: Binding -> Either Error Binding

rpsBinding b = do
  case bVal b of
    []  -> return b
    [_] -> return b
    xs  -> return b { bVal = rpsExpr xs }

---

rpsExpr
  :: [Expr Int] -> [Expr Int]

rpsExpr xs =
  let
    ys = if any ischeck xs then map addcheck xs else xs
    (zs,os) = partitionEithers $ map isOtherwise ys
    ncond p = Expr (BlnNot (orlist p $ map cond zs)) p
    os' = map (replace ncond) os
  in
    zs ++ os'

  where
    ischeck e = case expr e of
      Colon {} -> True
      _        -> False

    cond e = case expr e of
      Colon x _ -> x
      _         -> Expr BaseTrue (srcPos e)

    isOtherwise e = case expr e of
      Colon x _ -> case expr x of
        BaseOtherwise -> Right e
        _             -> Left e
      _        -> Left e

    addcheck e = case expr e of
      Colon {} -> e
      _        -> Expr (Colon (cond e) e) $ srcPos e

    replace f e = case expr e of
      Colon _ y -> Expr (Colon (f (srcPos e)) y) $ srcPos e
      _         -> e

    orlist p ys = foldl (fldor p) (Expr BaseFalse p) ys

    fldor p e1 e2 = Expr (BlnOr e1 e2) p

---
