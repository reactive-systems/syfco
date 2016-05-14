-----------------------------------------------------------------------------
-- |
-- Module      :  Info
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
-- 
-- Several printers to report information back to the user.
-- 
-----------------------------------------------------------------------------

module Info
    ( prTitle
    , prDescription
    , prSemantics
    , prTarget
    , prTags
    , prInfo           
    , prParameters
    , prInputs
    , prOutputs  
    , prVersion
    , prHelp         
    ) where

-----------------------------------------------------------------------------

import Data.Array
    ( (!)
    )  

import Data.Types
    ( Semantics(..)
    , Target(..)  
    )

import Config
    ( Configuration(..)
    )  

import Data.LTL
    ( Formula(..)
    , fmlInputs
    , fmlOutputs  
    )  

import Data.Error
    ( prError
    )  
    
import Data.Binding
    ( BindExpr(..)
    )
    
import Data.SymbolTable
    ( IdRec(..)  
    )
    
import Data.Specification
    ( Specification(..)
    )

import Writer.Eval
    ( eval
    )  

import Control.Monad
    ( unless
    )
      
import System.Environment
    ( getProgName
    )  

-----------------------------------------------------------------------------

-- | Prints the title of the given specification.

prTitle
  :: Specification -> IO ()

prTitle s =
  putStrLn $ title s

-----------------------------------------------------------------------------

-- | Prints the description of the given specification.

prDescription
  :: Specification -> IO ()

prDescription s =
  putStrLn $ description s

-----------------------------------------------------------------------------

-- | Prints the semantics of the given specification.

prSemantics 
  :: Specification -> IO ()

prSemantics s =
  putStrLn $ case semantics s of
    SemanticsMealy -> "Mealy"
    SemanticsMoore -> "Moore"
    SemanticsStrictMealy -> "Strict,Mealy"
    SemanticsStrictMoore -> "Strict,Moore"    
    
-----------------------------------------------------------------------------

-- | Prints the target of the given specification.

prTarget
  :: Specification -> IO ()

prTarget s =
  putStrLn $ case target s of
    TargetMealy -> "Mealy"
    TargetMoore -> "Moore"
    
-----------------------------------------------------------------------------

-- | Prints the tag list of the given specification.

prTags
  :: Specification -> IO ()

prTags s = case tags s of
  [] -> return ()
  xs -> putStrLn $ head xs ++ concatMap ((:) ' ' . (:) ',') (tail xs)

-----------------------------------------------------------------------------

-- | Prints the parameters of the given specification.  

prParameters
  :: Specification -> IO ()

prParameters s =
  let
    xs = map bIdent $ parameters s
    ys = map (idName . (symboltable s !)) xs
  in
    putStrLn $
    if null ys then ""
    else head ys ++ concatMap ((:) ',' . (:) ' ') (tail ys)

-----------------------------------------------------------------------------

-- | Prints the input signals of the given specification.

prInputs
  :: Configuration -> Specification -> IO ()

prInputs c s = case eval c s of
  Left err         -> prError err
  Right (es,ss,rs,as,is,gs) -> putStrLn $
    case fmlInputs $ And $ es ++ ss ++ rs ++ as ++ is ++ gs of
      (x:xr) -> x ++ concatMap ((:) ',' . (:) ' ') xr        
      []     -> ""

-----------------------------------------------------------------------------

-- | Prints the output signals of the given specification.

prOutputs
  :: Configuration -> Specification -> IO ()

prOutputs c s = case eval c s  of
  Left err         -> prError err
  Right (es,ss,rs,as,is,gs) -> putStrLn $
    case fmlOutputs $ And $ es ++ ss ++ rs ++ as ++ is ++ gs of
      (x:xr) -> x ++ concatMap ((:) ',' . (:) ' ') xr        
      []     -> ""
          
-----------------------------------------------------------------------------

-- | Prints the complete INFO section of the given specification.  

prInfo
  :: Specification -> IO ()

prInfo s = do
  putStrLn $ "Title:         \"" ++ title s ++ "\""
  putStrLn $ "Description:   \"" ++ description s ++ "\""
  putStr "Semantics:     "
  prSemantics s
  putStr "Target:        "
  prTarget s
  unless (null $ tags s) $ do
    putStr "Tags:          "
    prTags s

-----------------------------------------------------------------------------

-- | Prints the version and the program name.

prVersion
  :: IO ()

prVersion = do
  name <- getProgName
  putStrLn (name ++ " version 0.1.0.2")

-----------------------------------------------------------------------------

-- | Prints the help of the program.

prHelp
  :: IO ()

prHelp = do
  toolname <- getProgName
  mapM_ putStrLn 
    [ "Usage: " ++ toolname ++ " [OPTIONS]... <file>"
    , ""
    , "A Synthesis Format Converter to read and transform the high level synthesis format."
    , ""
    , "  File Operations:"
    , ""
    , "  -o, --output                    : Path of the output file (results are printed"
    , "                                    to STDOUT, if not set)"
    , "  -f, --format                    : Output format. Possible values are"
    , ""
    , "      * full [default]            : Input file with applied transformations"
    , "      * basic                     : High level format (without global section)"    
    , "      * utf8                      : Human readable output using UTF8 symbols"
    , "      * wring                     : Wring input format"
    , "      * lily                      : Lily input format"
    , "      * acacia                    : Acacia / Acacia+ input format"    
    , "      * ltlxba                    : LTL2BA / LTL3BA input format"
    , "      * promela                   : Promela LTL"
    , "      * unbeast                   : Unbeast input format"
    , "      * slugs                     : structured Slugs format [GR(1) only]"
    , "      * slugsin                   : SlugsIn format [GR(1) only]"
    , "      * psl                       : PSL Syntax"
    , "" 
    , "  -m, --mode                      : Output mode. Possible values are"
    , ""
    , "      * pretty [default]          : pretty printing (as less parentheses as possible)"
    , "      * fully                     : output fully parenthesized formulas"
    , ""
    , "  -pf, --part-file                : Create a partitioning (.part) file"
    , "  -bd, --bus-delimiter            : Delimiter used to print indexed bus signals"
    , "                                    (Default: '_')"
    , "  -in, --stdin                    : Read the input file from STDIN"
    , ""
    , "  File Modifications:"
    , ""
    , "  -os, --overwrite-semantics      : Overwrite the semantics of the file"
    , "  -ot, --overwrite-target         : Overwrite the target of the file"
    , "  -op, --overwrite-parameter      : Overwrite a parameter of the file"
    , ""
    , "  Formula Transformations (disabled by default):"
    , ""
    , "  -s0, --weak-simplify            : Simple simplifications (removal of true, false in "
    , "                                    boolean connectives, redundant temporal operator,"
    , "                                    etc.)"
    , "  -s1, --strong-simplify          : Advanced simplifications (includes: -s0 -nnf -nw"
    , "                                    -nr -lgo -lfo -lxo)"
    , "  -nnf, --negation-normal-form    : Convert the resulting LTL formula into negation"
    , "                                    normal form"
    , "  -pgi, --push-globally-inwards   : Push global operators inwards"
    , "                                      G (a && b) => (G a) && (G b)"
    , "  -pfi, --push-finally-inwards    : Push finally operators inwards"
    , "                                      F (a || b) => (F a) || (F b)"
    , "  -pxi, --push-next-inwards       : Push next operators inwards"
    , "                                      X (a && b) => (X a) && (X b)"
    , "                                      X (a || b) => (X a) || (X b)"
    , "  -pgo, --pull-globally-outwards  : Pull global operators outwards"
    , "                                      (G a) && (G b) => G (a && b)"
    , "  -pfo, --pull-finally-outwards   : Pull finally operators outwards"
    , "                                      (F a) || (F b) => G (a || b)"
    , "  -pxo, --pull-next-outwards      : Pull next operators outwards"
    , "                                      (X a) && (X b) => X (a && b)"
    , "                                      (X a) || (X b) => X (a && b)"
    , "  -nw, --no-weak-until            : Replace weak until operators"
    , "                                      a W b => (a U b) || (G a)"
    , "  -nr, --no-release               : Replace release operators"
    , "                                      a R b => b W (a && b)"
    , "  -nf, --no-finally               : Replace finally operators"
    , "                                      F a => true U a"  
    , "  -ng, --no-globally              : Replace global operators"
    , "                                      G a => false R a"
    , "  -nd, --no-derived               : Same as: -nw -nf -ng"
    , ""
    , "  Check Secification Type (and exit):"
    , ""
    , "  -gr                             : Check whether the input is in the"
    , "                                    Generalized Reactivity fragment"      
    , ""      
    , "  Extract Information (and exit):"
    , ""
    , "  -c, --check                     : Check that input conforms to TLSF"
    , "  -t, --print-title               : Output the title of the input file"
    , "  -d, --print-description         : Output the description of the input file"
    , "  -s, --print-semantics           : Output the semantics of the input file"
    , "  -g, --print-target              : Output the target of the input file"
    , "  -a, --print-tags                : Output the target of the input file"    
    , "  -p, --print-parameters          : Output the parameters of the input file"
    , "  -i, --print-info                : Output all data of the info section"    
    , "  -ins, --print-input-signals     : Output the input signals of the specification"
    , "  -outs, --print-output-signals   : Output the output signals of the specification"
    , ""
    , "  -v, --version                   : Output version information"
    , "  -h, --help                      : Display this help"
    , ""
    , "Sample usage:"
    , ""
    , "  " ++ toolname ++ " -o converted -f promela -m fully -nnf -nd file.tlsf"
    , "  " ++ toolname ++ " -f psl -op n=3 -os Strict,Mealy -o converted file.tlsf"
    , "  " ++ toolname ++ " -o converted -in"
    , "  " ++ toolname ++ " -t file.tlsf"
    , ""
    ]  

-----------------------------------------------------------------------------
