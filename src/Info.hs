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
  Right (as,is,gs) -> putStrLn $
    case fmlInputs $ And $ as ++ is ++ gs of
      (x:xr) -> x ++ concatMap ((:) ',' . (:) ' ') xr        
      []     -> ""

-----------------------------------------------------------------------------

-- | Prints the output signals of the given specification.

prOutputs
  :: Configuration -> Specification -> IO ()

prOutputs c s = case eval c s  of
  Left err         -> prError err
  Right (as,is,gs) -> putStrLn $
    case fmlOutputs $ And $ as ++ is ++ gs of
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
  putStrLn (name ++ " version 0.1.0.0")

-----------------------------------------------------------------------------

-- | Prints the help of the program.

prHelp
  :: IO ()

prHelp = do
  toolname <- getProgName
  putStrLn 
    ("Usage: " ++ toolname ++ " [OPTIONS]... <file>"
    ++ "\n"
    ++ "\n" ++ "A Synthesis Format Converter to read and transform the high level synthesis format."
    ++ "\n"
    ++ "\n" ++ "  File Operations:"
    ++ "\n"
    ++ "\n" ++ "  -o, --output                    : Path of the output file (results are printed"
    ++ "\n" ++ "                                    to STDOUT, if not set)"
    ++ "\n" ++ "  -f, --format                    : Output format. Possible values are"
    ++ "\n"
    ++ "\n" ++ "      * full [default]            : Input file with applied transformations"
    ++ "\n" ++ "      * basic                     : High level format (without global section)"    
    ++ "\n" ++ "      * utf8                      : Human readable output using UTF8 symbols"
    ++ "\n" ++ "      * wring                     : Wring input format"
    ++ "\n" ++ "      * lily                      : Lily input format"
    ++ "\n" ++ "      * acacia                    : Acacia / Acacia+ input format"    
    ++ "\n" ++ "      * ltlxba                    : LTL2BA / LTL3BA input format"
    ++ "\n" ++ "      * promela                   : Promela LTL"
    ++ "\n" ++ "      * unbeast                   : Unbeast input format"    
    ++ "\n" ++ "      * psl                       : PSL Syntax"
    ++ "\n" 
    ++ "\n" ++ "  -m, --mode                      : Output mode. Possible values are"
    ++ "\n"
    ++ "\n" ++ "      * pretty [default]          : pretty printing (as less parentheses as possible)"
    ++ "\n" ++ "      * fully                     : output fully parenthesized formulas"
    ++ "\n"
    ++ "\n" ++ "  -pf, --part-file                : Create a partitioning (.part) file"
    ++ "\n" ++ "  -bd, --bus-delimiter            : Delimiter used to print indexed bus signals"
    ++ "\n" ++ "                                    (Default: '_')"
    ++ "\n" ++ "  -in, --stdin                    : Read the input file from STDIN"
    ++ "\n"
    ++ "\n" ++ "  File Modifications:"
    ++ "\n"
    ++ "\n" ++ "  -os, --overwrite-semantics      : Overwrite the semantics of the file"
    ++ "\n" ++ "  -ot, --overwrite-target         : Overwrite the target of the file"
    ++ "\n" ++ "  -op, --overwrite-parameter      : Overwrite a parameter of the file"
    ++ "\n"
    ++ "\n" ++ "  Formula Transformations (disabled by default):"
    ++ "\n"
    ++ "\n" ++ "  -s0, --weak-simplify            : Simple simplifications (removal of true, false in "
    ++ "\n" ++ "                                    boolean connectives, redundant temporal operator,"
    ++ "\n" ++ "                                    etc.)"
    ++ "\n" ++ "  -s1, --strong-simplify          : Advanced simplifications (includes: -s0 -nnf -nw"
    ++ "\n" ++ "                                    -nr -lgo -lfo -lxo)"
    ++ "\n" ++ "  -nnf, --negation-normal-form    : Convert the resulting LTL formula into negation"
    ++ "\n" ++ "                                    normal form"
    ++ "\n" ++ "  -pgi, --push-globally-inwards   : Push global operators inwards"
    ++ "\n" ++ "                                      G (a && b) => (G a) && (G b)"
    ++ "\n" ++ "  -pfi, --push-finally-inwards    : Push finally operators inwards"
    ++ "\n" ++ "                                      F (a || b) => (F a) || (F b)"
    ++ "\n" ++ "  -pxi, --push-next-inwards       : Push next operators inwards"
    ++ "\n" ++ "                                      X (a && b) => (X a) && (X b)"
    ++ "\n" ++ "                                      X (a || b) => (X a) || (X b)"
    ++ "\n" ++ "  -pgo, --pull-globally-outwards  : Pull global operators outwards"
    ++ "\n" ++ "                                      (G a) && (G b) => G (a && b)"
    ++ "\n" ++ "  -pfo, --pull-finally-outwards   : Pull finally operators outwards"
    ++ "\n" ++ "                                      (F a) || (F b) => G (a || b)"
    ++ "\n" ++ "  -pxo, --pull-next-outwards      : Pull next operators outwards"
    ++ "\n" ++ "                                      (X a) && (X b) => X (a && b)"
    ++ "\n" ++ "                                      (X a) || (X b) => X (a && b)"
    ++ "\n" ++ "  -nw, --no-weak-until            : Replace weak until operators"
    ++ "\n" ++ "                                      a W b => (a U b) || (G a)"
    ++ "\n" ++ "  -nr, --no-release               : Replace release operators"
    ++ "\n" ++ "                                      a R b => b W (a && b)"
    ++ "\n" ++ "  -nf, --no-finally               : Replace finally operators"
    ++ "\n" ++ "                                      F a => true U a"  
    ++ "\n" ++ "  -ng, --no-globally              : Replace global operators"
    ++ "\n" ++ "                                      G a => false R a"
    ++ "\n" ++ "  -nd, --no-derived               : Same as: -nw -nf -ng"
    ++ "\n"
    ++ "\n" ++ "  Print Information (and exit):"
    ++ "\n"
    ++ "\n" ++ "  -c, --check                     : Check the input file"
    ++ "\n" ++ "  -t, --print-title               : Output the title of the input file"
    ++ "\n" ++ "  -d, --print-description         : Output the description of the input file"
    ++ "\n" ++ "  -s, --print-semantics           : Output the semantics of the input file"
    ++ "\n" ++ "  -g, --print-target              : Output the target of the input file"
    ++ "\n" ++ "  -a, --print-tags                : Output the target of the input file"    
    ++ "\n" ++ "  -p, --print-parameters          : Output the parameters of the input file"
    ++ "\n" ++ "  -i, --print-info                : Output all data of the info section"    
    ++ "\n" ++ "  -ins, --print-input-signals     : Output the input signals of the specification"
    ++ "\n" ++ "  -outs, --print-output-signals   : Output the output signals of the specification"
    ++ "\n"
    ++ "\n" ++ "  -v, --version                   : Output version information"
    ++ "\n" ++ "  -h, --help                      : Display this help"
    ++ "\n"
    ++ "\n" ++ "Sample usage:"
    ++ "\n"
    ++ "\n" ++ "  " ++ toolname ++ " -o converted -f promela -m fully -nnf -nd file.tlsf"
    ++ "\n" ++ "  " ++ toolname ++ " -f psl -op n=3 -os Strict,Mealy -o converted file.tlsf"
    ++ "\n" ++ "  " ++ toolname ++ " -o converted -in"
    ++ "\n" ++ "  " ++ toolname ++ " -t file.tlsf"
    ++ "\n")

-----------------------------------------------------------------------------
