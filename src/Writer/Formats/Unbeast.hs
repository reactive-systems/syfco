-----------------------------------------------------------------------------
-- |
-- Module      :  Writer.Formats.Unbeast
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
-- 
-- Transforms a specification to the Unbeast format.
-- 
-----------------------------------------------------------------------------

module Writer.Formats.Unbeast
    ( writeUnbeast
    ) where

-----------------------------------------------------------------------------

import Config
import Simplify

import Data.LTL
import Data.Error
import Data.Specification

import Writer.Eval
import Writer.Data

-----------------------------------------------------------------------------

-- | Unbeast format writer.

writeUnbeast
  :: Configuration -> Specification -> Either Error WriteContents

writeUnbeast c s = do
  (as,is,gs) <- eval d s
  as' <- mapM (simplify c) as
  vs' <- mapM (simplify c) $ case (is,gs) of
    ([],[])   -> []
    ([],[x])  -> [x]
    ([],_)    -> gs
    ([x],[])  -> [Globally x]
    ([x],[y]) -> [Globally x,y]
    ([x],_)   -> ((Globally x) : gs)
    (_,[])    -> [Globally $ And is]
    (_,[x])   -> [Globally $ And is, x]
    (_,_)     -> ((Globally $ And is) : gs)
    
  return $ WriteContents {
    mainFile = main as' vs',
    partitionFile = Nothing
    }

  where
    main as vs =
      "<?xml version=\"1.0\" encoding=\"ISO-8859-1\" ?>" 
      ++ "\n" ++ "<!DOCTYPE SynthesisProblem SYSTEM \"" ++ specfile ++ "\">"
      ++ "\n" ++ "<!--"
      ++ "\n" ++ "This specification was automatically created from a high level specification,"
      ++ "\n" ++ "using the SyFCo tool."
      ++ "\n"
      ++ "\n" ++ "Please consider that default values for the .dtd file and the LTL compiler"
      ++ "\n" ++ "have been used. To change them, you can use the 'updatePathsInXML.py' script,"
      ++ "\n" ++ "that is shipped with the Unbeast tool."
      ++ "\n" ++ "-->"
      ++ "\n" ++ "<SynthesisProblem>"
      ++ "\n" ++ "  <Title>" ++ title s ++ "</Title>"
      ++ "\n" ++ "  <Description>" ++ description s ++ "</Description>"
      ++ "\n" ++ "  <PathToLTLCompiler>" ++ compiler ++ "</PathToLTLCompiler>"
      ++ "\n" ++ "  <GlobalInputs>"
      ++ (concatMap printSignal $ fmlInputs $ Implies (And as) (And vs))
      ++ "\n" ++ "  </GlobalInputs>"  
      ++ "\n" ++ "  <GlobalOutputs>"
      ++ (concatMap printSignal $ fmlOutputs $ Implies (And as) (And vs))
      ++ "\n" ++ "  </GlobalOutputs>"
      ++ (if null as then "" 
          else "\n" ++ "  <Assumptions>" ++ (concatMap printFormula as) ++ "\n" ++ "  </Assumptions>")
      ++ "\n" ++ "  <Specification>"
      ++ (concatMap printFormula vs)
      ++ "\n" ++ "  </Specification>"
      ++ "\n" ++ "</SynthesisProblem>"
      ++ "\n"
    
    specfile = "SynSpec.dtd"
    compiler = "ltl2ba -f"

    d = busDelimiter c    

    printSignal sig =
      "\n    <Bit>" ++ sig ++ "</Bit>"

    printFormula f = case f of
      TTrue       -> "<True>"
      FFalse      -> "<False>"
      Atomic x    -> "<Var>" ++ show x ++ "</Var>"
      Not x       -> "<Not>" ++ printFormula x ++ "</Not>"
      Next x      -> "<X>" ++ printFormula x ++ "</X>"
      Globally x  -> "<G>" ++ printFormula x ++ "</G>"
      Finally x   -> "<F>" ++ printFormula x ++ "</F>"
      Or xs       -> "<Or>" ++ concatMap printFormula xs ++ "</Or>"
      And xs      -> "<And>" ++ concatMap printFormula xs ++ "</And>"
      Equiv x y   -> "<Iff>" ++ printFormula x ++ printFormula y ++ "</Iff>"
      Until x y   -> "<U>" ++ printFormula x ++ printFormula y ++ "</U>"
      Weak x y    -> "<WU>" ++ printFormula x ++ printFormula y ++ "</WU>"
      Implies _ _ -> error "Unbeast does not support the implication operator"      
      Release _ _ -> error "Unbeast does not support the release operator"

-----------------------------------------------------------------------------


