-----------------------------------------------------------------------------
-- |
-- Module      :  Reader.Sugar
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- Removes syntactic sugar elements from the specification.
--
-----------------------------------------------------------------------------

module Reader.Sugar
  ( replaceSugar
  ) where

-----------------------------------------------------------------------------

import Reader.Error
  ( Error
  )

import Data.Binding
  ( Binding
  , BindExpr(..)
  )

import Reader.Data
  ( Specification(..)
  )

import Data.Expression
  ( Expr(..)
  , Expr'(..)
  )

import Data.Either
  ( partitionEithers
  )

-----------------------------------------------------------------------------

-- | Replaces syntactic sugar elements in the given specification by their
-- corresponding standard elements.

replaceSugar
  :: Specification -> Either Error Specification

replaceSugar s = do
  vs <- mapM replaceBinding $ definitions s
  return s { definitions = vs }

-----------------------------------------------------------------------------

replaceBinding
  :: Binding -> Either Error Binding

replaceBinding b =
  case bVal b of
    []  -> return b
    [_] -> return b
    xs  -> return b { bVal = replaceExpr xs }

-----------------------------------------------------------------------------

replaceExpr
  :: [Expr Int] -> [Expr Int]

replaceExpr xs =
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

    orlist p = foldl (fldor p) (Expr BaseFalse p)

    fldor p e1 e2 = Expr (BlnOr e1 e2) p

-----------------------------------------------------------------------------
