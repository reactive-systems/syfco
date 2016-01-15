-----------------------------------------------------------------------------
-- |
-- Module      :  Writer.Formats.Unbeast
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
-- 
-- Transforms a specification to the Unbeast format.
-- 
-----------------------------------------------------------------------------

module Writer.Formats.Unbeast where

-----------------------------------------------------------------------------

import Config
import Simplify

import Data.LTL
import Data.Error
import Data.Specification

import Writer.Eval
import Control.Exception

-----------------------------------------------------------------------------

-- | Unbeast format writer.

writeFormat
  :: Configuration -> Specification -> Either Error String

writeFormat c s = do
  (as,is,gs) <- eval c s
  as' <- mapM (simplify' (c { noRelease = True, noWeak = True })) as
  vs' <- mapM (simplify' (c { noRelease = True, noWeak = True })) $
         case (is, gs) of
           ([],[])   -> []
           ([],[x])  -> [x]
           ([],_)    -> gs
           ([x],[])  -> [Globally x]
           ([x],[y]) -> [Globally x,y]
           ([x],_)   -> Globally x : gs
           (_,[])    -> [Globally $ And is]
           (_,[x])   -> [Globally $ And is, x]
           (_,_)     -> (Globally $ And is) : gs

  return $ main as' vs'

  where
    main as vs = 
                 "<?xml version=\"1.0\" encoding=\"ISO-8859-1\" ?>" 
      ++ "\n" ++ "<!DOCTYPE SynthesisProblem SYSTEM \""
              ++ specfile ++ "\">"
      ++ "\n"
      ++ "\n" ++ "<!--"
      ++ "\n" ++ "This specification was automatically created "
              ++ "from a TLSF specification,"
      ++ "\n" ++ "using the SyFCo tool."
      ++ "\n"
      ++ "\n" ++ "Please consider that default values for the .dtd file "
              ++ "and the LTL compiler"
      ++ "\n" ++ "have been used. To change them, you can use the "
              ++ "'updatePathsInXML.py' script,"
      ++ "\n" ++ "that is shipped with the Unbeast tool."
      ++ "\n" ++ "-->"
      ++ "\n"
      ++ "\n" ++ "<SynthesisProblem>"
      ++ "\n" ++ "  <Title>" ++ title s ++ "</Title>"
      ++ "\n" ++ "  <Description>" 
      ++ "\n" ++ fixedIndent (description s)
      ++ "\n" ++ "  </Description>"
      ++ "\n" ++ "  <PathToLTLCompiler>" ++ compiler
              ++ "</PathToLTLCompiler>"
      ++ "\n" ++ "  <GlobalInputs>"
      ++ concatMap printSignal (fmlInputs $ Implies (And as) (And vs))
      ++ "\n" ++ "  </GlobalInputs>"  
      ++ "\n" ++ "  <GlobalOutputs>"
      ++ concatMap printSignal (fmlOutputs $ Implies (And as) (And vs))
      ++ "\n" ++ "  </GlobalOutputs>"
      ++ (if null as then "" 
          else "\n" ++ "  <Assumptions>" ++
               concatMap (\x -> "\n    <LTL>\n" ++ printFormula 6 x
                               ++ "    </LTL>\n") as ++
               "  </Assumptions>")
      ++ "\n" ++ "  <Specification>"
      ++ concatMap (\x -> "\n    <LTL>\n" ++ printFormula 6 x
                         ++ "    </LTL>\n") vs
      ++ "  </Specification>"
      ++ "\n" ++ "</SynthesisProblem>"
      ++ "\n"
    
    specfile = "SynSpec.dtd"
    compiler = "ltl2ba -f"

    fixedIndent str = case str of
      []        -> []
      (' ':xr)  -> fixedIndent xr
      ('\t':xr) -> fixedIndent xr
      ('\n':xr) -> fixedIndent xr
      _         -> "    " ++
                 concatMap ident (rmLeadingSpace [] False str)

    ident chr = case chr of
      '\n' -> "\n    "
      _    -> [chr]

    rmLeadingSpace a b str = case str of
      []        -> reverse a
      ('\n':xr) -> rmLeadingSpace ('\n':a) True xr
      ('\t':xr) ->
        if b
        then rmLeadingSpace a b xr
        else rmLeadingSpace (' ':a) b xr
      (' ':xr)  ->
        if b
        then rmLeadingSpace a b xr
        else rmLeadingSpace (' ':a) b xr             
      (x:xr)    -> rmLeadingSpace (x:a) False xr            
        
    printSignal sig =
      "\n    <Bit>" ++ sig ++ "</Bit>"

    printFormula n f = replicate n ' ' ++ printFormula' (n + 2) f
    
    printFormula' n f = case f of
      TTrue       -> "<True></True>\n"
      FFalse      -> "<False></False>\n"
      Atomic x    -> "<Var>" ++ show x ++ "</Var>\n"
      Not x       -> "<Not>\n" ++ printFormula n x ++ replicate (n - 2) ' ' ++ "</Not>\n"
      Next x      -> "<X>\n" ++ printFormula n x ++ replicate (n - 2) ' ' ++ "</X>\n"
      Globally x  -> "<G>\n" ++ printFormula n x ++ replicate (n - 2) ' ' ++ "</G>\n"
      Finally x   -> "<F>\n" ++ printFormula n x ++ replicate (n - 2) ' ' ++ "</F>\n"
      Or xs       -> "<Or>\n" ++ concatMap (printFormula n) xs ++ replicate (n - 2) ' ' ++ "</Or>\n"
      And xs      -> "<And>\n" ++ concatMap (printFormula n) xs ++ replicate (n - 2) ' ' ++ "</And>\n"
      Equiv x y   -> "<Iff>\n" ++ printFormula n x ++ printFormula n y ++ replicate (n - 2) ' ' ++ "</Iff>\n"
      Until x y   -> "<U>\n" ++ printFormula n x ++ printFormula n y ++ replicate (n - 2) ' ' ++ "</U>\n"
      _           -> assert False undefined

    noImpl fml = case fml of
      Implies x y -> Or [Not $ noImpl x, noImpl y]
      _           -> applySub noImpl fml

    simplify' cc f = do
      f' <- simplify cc $ noImpl f
      if f' == f then return f else simplify' cc f'

-----------------------------------------------------------------------------
