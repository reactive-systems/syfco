-----------------------------------------------------------------------------
-- |
-- Module      :  Info
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- Several printers to report information back to the user.
--
-----------------------------------------------------------------------------

{-# LANGUAGE

    LambdaCase
  , MultiParamTypeClasses
  , TypeSynonymInstances
  , FlexibleContexts

  #-}

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
  , prReadme
  , prReadmeMd
  , prError
  ) where

-----------------------------------------------------------------------------

import Syfco
  ( Configuration(..)
  , Semantics(..)
  , Target(..)
  , WriteFormat(..)
  , WriteMode(..)
  , Specification
  , defaultCfg
  , title
  , description
  , semantics
  , target
  , tags
  , parameters
  , inputs
  , outputs
  , version
  )

import Data.Convertible
  ( Convertible
  , convert
  )

import Data.Array
  ( (!)
  )

import Control.Monad
  ( unless
  )

import System.Exit
  ( exitFailure
  )

import System.IO
  ( hPutStrLn
  , stderr
  )

-----------------------------------------------------------------------------

data Mode = Plain | Help | Markdown

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

prParameters s = putStrLn $ case parameters s of
  (x:xr) -> x ++ concatMap ((:) ',' . (:) ' ') xr
  []     -> ""

-----------------------------------------------------------------------------

-- | Prints the input signals of the given specification.

prInputs
  :: Configuration -> Specification -> IO ()

prInputs c s = case inputs c s of
  Left err     -> prError $ show err
  Right (x:xr) -> putStrLn $ x ++ concatMap ((:) ',' . (:) ' ') xr
  _            -> return ()

-----------------------------------------------------------------------------

-- | Prints the output signals of the given specification.

prOutputs
  :: Configuration -> Specification -> IO ()

prOutputs c s = case outputs c s of
  Left err     -> prError $ show err
  Right (x:xr) -> putStrLn $ x ++ concatMap ((:) ',' . (:) ' ') xr
  _            -> return ()

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
  putStrLn $ "SyFCo (v" ++ version ++ ")"
  putStrLn "The Synthesis Format Converter"

-----------------------------------------------------------------------------

-- | Prints the help of the program.

prHelp
  :: IO ()

prHelp = putStr $ usage Help

-----------------------------------------------------------------------------

-- | Prints the content of the README file.

prReadme
  :: IO ()

prReadme = putStr $ readme Plain

-----------------------------------------------------------------------------

-- | Prints the content of the README.md file.

prReadmeMd
  :: IO ()

prReadmeMd = putStr $ readme Markdown

-----------------------------------------------------------------------------

usage
  :: Mode -> String

usage m =
  unlines $
    [ switch "## Usage\n\n" "Usage: " "## Usage\n\n" ++
      code m ("syfco [OPTIONS]... <file>") ++
      switch "\n" ("\n\nA Synthesis Format Converter to read and " ++
                   "transform Temporal Logic\nSpecification Format " ++
                   "(TLSF) files.") "" ] ++
    section "File Operations" (prTable foTable) ++
    section "File Modifications" (prTable fmTable) ++
    section ("Formula Transformations " ++
             "(disabled by default)") (prTable ftTable) ++
    section "Check Specification Type (and exit)" (prTable csTable) ++
    section "Extract Information (and exit)" (prTable eiTable) ++
    section "Sample Usage" sample

  where
    idl (s1,s2,_,_) = length s1 + length s2 + 7

    len = foldl max 0 $
      [ foldl max 0 $ map idl foTable
      , foldl max 0 $ map idl fmTable
      , foldl max 0 $ map idl ftTable
      , foldl max 0 $ map idl csTable
      , foldl max 0 $ map idl eiTable ]

    switch s1 s2 s3 = case m of
      Plain    -> s1
      Help     -> s2
      Markdown -> s3

    section h xs = [ "", ind ++ h ++ ":", "" ] ++ xs

    ind = case m of
      Plain    -> "### "
      Help     -> ""
      Markdown -> "#### "

    wrap str = case m of
      Markdown -> code m str
      _        -> "`" ++ str ++ "`"

    sample = [ codeblock m $ map (("syfco ") ++)
      [ "-o converted -f promela -m fully -nnf -nd file.tlsf"
      , "-f psl -op n=3 -os Strict,Mealy -o converted file.tlsf"
      , "-o converted -in"
      , "-t file.tlsf" ] ]

    prTable xs =
      let xs' =
            map (\(x,y,z,vs) ->
                (x,y,adaptsub (80 - len - 3) z,
                 adapt (80 - len - 3) vs)) xs
      in case m of
        Markdown ->
          [ "|Command|Description|"
          , "|-------|-----------|" ]
          ++ map prMRow (filter (\(s,_,_,_) -> not $ null s) xs')
        _        ->
          concatMap prRow xs'

    adaptsub l c = case c of
      Nothing -> Nothing
      Just xs -> Just $ map (\(a,b,c) -> (a,b,adapt l c)) xs

    adapt
      :: Int -> [String] -> [String]

    adapt l xs = concatMap (adapt' l) xs

    adapt' l str
      | length str <= l = [str]
      | otherwise      = case m of
        Markdown -> [str]
        _        -> rearrange l [] [] 0 $ words str

    rearrange l a b n [] =
      reverse ((unwords $ reverse b):a)
    rearrange l a [] _ (x:xr)
      | length x > l = rearrange l (x:a) [] 0 xr
      | otherwise    = rearrange l a [x] (length x) xr
    rearrange l a b n (x:xr)
      | n + length x + 1 > l =
        rearrange l ((unwords $ reverse b):a) [x] (length x) xr
      | otherwise =  rearrange l a (x:b) (n + length x + 1) xr

    prMRow (short,long,sub,desc) =
      "|" ++ code m ("-" ++ short ++ ", --" ++ long) ++ "|" ++
      ( case desc of
           []   -> ""
           x:xr -> concat (x : map ("</br> " ++) xr) ++
                  case sub of
                    Nothing -> ""
                    Just ys -> "</br> <table><tbody> " ++
                              concatMap prMSub ys ++
                              " </tbody></table>"
      ) ++ "|"

    prMSub (name,d,desc) =
      "<tr><td>" ++ code m name ++
      "</td><td>" ++ concat (addbreaks desc) ++
      (if d then " (default)" else "") ++
      "</td></tr>"

    addbreaks xs = case xs of
      []   -> []
      x:xr -> x : map ("</br>" ++) xr

    prRow (short,long,sub,desc)
      | short == "" = [ "" ]
      | otherwise  = case desc of
        []   ->
          prRow (short,long,sub,[""])
        x:xr ->
          ( ( "  -" ++ short ++ ", --" ++ long ++
              replicate (len - (length short + length long + 7)) ' '
              ++ " : " ++ x)
            : ( [ replicate (len + 3) ' ' ++ y | y <- xr ] ++
                (case sub of
                    Nothing -> []
                    Just ys -> [""] ++ concatMap prSub ys ++ [""] ) ) )

    prSub (n,d,desc) = case desc of
      []   -> prSub (n,d,[""])
      x:xr ->
        ("    * " ++ n ++
         (if d then
            " [default]" ++ replicate (len - 16 - length n) ' '
          else
            replicate (len - 6 - length n) ' ') ++
         " : " ++ x) :
        map ((replicate (len+3) ' ') ++) xr

    foTable =
      [ ("o", "output", Nothing,
         [ "path of the output file (results are printed " ++
           "to STDOUT if not set)" ])
      , ("r", "read-config", Nothing,
         [ "read parameters from the given configuration file (may " ++
           "overwrite prior arguments)" ])
      , ("w", "write-config", Nothing,
         [ "write the current configuration to the given path " ++
           "(includes later arguments)" ])
      , ("f", "format", Just formats,
         [ "output format - possible values are:" ])
      , ("m", "mode", Just modes,
         [ "output mode - possible values are:" ])
      , ("pf", "part-file", Nothing,
         [ "create a partitioning (" ++ code m ".part" ++ ") file" ])
      , ("bd", "bus-delimiter", Nothing,
         [ "delimiter used to print indexed bus signals",
           "(default: "++ wrap (busDelimiter defaultCfg) ++ ")" ])
      , ("ps", "prime-symbol", Nothing,
         [ "symbol/string denoting primes in signals",
           "(default: " ++ wrap (primeSymbol defaultCfg) ++ ")" ])
      , ("as", "at-symbol", Nothing,
         [ "symbol/string denoting @-symbols in signals",
           "(default: " ++ wrap (atSymbol defaultCfg) ++ ")" ])
      , ("in", "stdin", Nothing,
         [ "read the input file from STDIN" ]) ]

    fmTable =
      [ ("os", "overwrite-semantics", Nothing,
         [ "overwrite the semantics of the file" ])
      , ("ot", "overwrite-target", Nothing,
         [ "overwrite the target of the file" ])
      , ("op", "overwrite-parameter", Nothing,
         [ "overwrite a parameter of the file" ]) ]

    ftTable =
      [ ("s0", "weak-simplify", Nothing,
         [ "simple simplifications (removal of true/false " ++
           "in boolean connectives, redundant temporal " ++
           "operators, etc.)" ])
      , ("s1", "strong-simplify", Nothing,
         [ "advanced simplifications",
           "(includes: " ++
           code m "-s0 -nnf -nw -nr -pgo -pfo -pxo" ++ ")" ])
      , ("nnf", "negation-normal-form", Nothing,
         [ "convert the resulting LTL formula into negation " ++
           "normal form" ])
      , ("pgi", "push-globally-inwards", Nothing,
         [ "push global operators inwards",
           "  " ++ code m "G (a && b) => (G a) && (G b)" ])
      , ("pfi", "push-finally-inwards", Nothing,
         [ "push finally operators inwards",
           "  " ++ code m "F (a || b) => (F a) || (F b)" ])
      , ("pxi", "push-next-inwards", Nothing,
         [ "push next operators inwards",
           "  " ++ code m "X (a && b) => (X a) && (X b)",
           "  " ++ code m "X (a || b) => (X a) || (X b)" ])
      , ("pgo", "pull-globally-outwards", Nothing,
         [ "pull global operators outwards",
           "  " ++ code m "(G a) && (G b) => G (a && b)" ])
      , ("pfo", "pull-finally-outwards", Nothing,
         [ "pull finally operators outwards",
           "  " ++ code m "(F a) || (F b) => F (a || b)" ])
      , ("pxo", "pull-next-outwards", Nothing,
         [ "pull next operators outwards",
           "  " ++ code m "(X a) && (X b) => X (a && b)",
           "  " ++ code m "(X a) || (X b) => X (a || b)" ])
      , ("nw", "no-weak-until", Nothing,
         [ "replace weak until operators",
           "  " ++ code m "a W b => (a U b) || (G a)" ])
      , ("nr", "no-release", Nothing,
         [ "replace release operators",
           "  " ++ code m "a R b => b W (a && b)" ])
      , ("nf", "no-finally", Nothing,
         [ "replace finally operators",
           "  " ++ code m "F a => true U a" ])
      , ("ng", "no-globally", Nothing,
         [ "replace global operators",
           "  " ++ code m "G a => false R a" ])
      , ("nd", "no-derived", Nothing,
         [ "same as: " ++ code m "-nw -nf -ng" ]) ]

    csTable =
      [ ("gr", "generalized-reactivity", Nothing,
         [ "check whether the input is in the " ++
           "Generalized Reactivity fragment" ]) ]

    eiTable =
      [ ("c", "check", Nothing,
         [ "check that input conforms to TLSF" ])
      , ("t", "print-title", Nothing,
         [ "output the title of the input file" ])
      , ("d", "print-description", Nothing,
         [ "output the description of the input file" ])
      , ("s", "print-semantics", Nothing,
         [ "output the semantics of the input file" ])
      , ("g", "print-target", Nothing,
         [ "output the target of the input file" ])
      , ("a", "print-tags", Nothing,
         [ "output the target of the input file" ])
      , ("p", "print-parameters", Nothing,
         [ "output the parameters of the input file" ])
      , ("i", "print-info", Nothing,
         [ "output all data of the info section" ])
      , ("ins", "print-input-signals", Nothing,
         [ "output the input signals of the specification" ])
      , ("outs", "print-output-signals", Nothing,
         [ "output the output signals of the specification" ])
      , ("","", Nothing,
         [])
      , ("v", "version", Nothing,
         [ "output version information" ])
      , ("h", "help", Nothing,
         [ "display this help" ]) ]

    formats =
      [ (convert FULL, True,
         ["input file with applied transformations"])
      , (convert BASIC, False,
         ["high level format (without global section)"])
      , (convert UTF8, False,
         ["human readable output using UTF8 symbols"])
      , (convert WRING, False,
         ["Wring input format"])
      , (convert LILY, False,
         ["Lily input format"])
      , (convert ACACIA, False,
         ["Acacia / Acacia+ input format"])
      , (convert ACACIASPECS, False,
         ["Acacia input format with spec units"])
      , (convert LTLXBA, False,
         ["LTL2BA / LTL3BA input format"])
      , (convert PROMELA, False,
         ["Promela LTL"])
      , (convert UNBEAST, False,
         ["Unbeast input format"])
      , (convert SLUGS, False,
         ["structured Slugs format [GR(1) only]"])
      , (convert SLUGSIN, False,
         ["SlugsIn format [GR(1) only]"])
      , (convert PSL, False,
         ["PSL Syntax"])
      , (convert SMV, False,
         ["SMV file format"])
      , (convert BOSY, False,
         ["Bosy input format"])
      ]

    modes =
      [ (convert Pretty, True,
         ["pretty printing (as less parentheses as possible)"])
      , (convert Fully, False,
         ["output fully parenthesized formulas"]) ]

-----------------------------------------------------------------------------

readme
  :: Mode -> String

readme m = appendlinks $ unlines
  [ switch
      ("# Synthesis Format Conversion Tool\n# (Version " ++
       version ++ ")")
      ("# Synthesis Format Conversion Tool<br/>(Version " ++
       version ++ ")")
  , ""
  , "A tool for reading, manipulating and transforming synthesis"
  , "specifications in " ++
    link "TLSF" "https://arxiv.org/abs/1604.02284" ++ "."
  , ""
  , "## About this tool"
  , ""
   , "The tool interprets the high level constructs of " ++
    link "TLSF 1.1" "https://arxiv.org/abs/1604.02284"
  , "(functions, sets, ...) and supports the transformation of the"
  , "specification to Linear Temporal Logic (LTL) in different output"
  , "formats. The tool has been designed to be modular with respect to the"
  , "supported output formats and semantics. Furthermore, the tool allows"
  , "to identify and manipulate parameters, targets and semantics of a"
  , "specification on the fly. This is especially thought to be useful for"
  , "comparative studies, as they are for example needed in the"
  , link "Synthesis Competition" "http://www.syntcomp.org" ++ "."
  , ""
  , "The main features of the tool are summarized as follows:"
  , ""
  , "* Interpretation of high level constructs, which allows to reduce the"
  , "  specification to its basic fragment where no more parameter and"
  , "  variable bindings occur (i.e., without the GLOBAL section)."
  , ""
  , "* Transformation to other existing specification formats, like"
  , "  Basic TLSF, " ++
    link "Promela LTL" "http://spinroot.com/spin/Man/ltl.html" ++ ", " ++
    link "PSL" ("https://en.wikipedia.org/wiki/" ++
                "Property_Specification_Language")
    ++ ", " ++
    link "Unbeast" "https://www.react.uni-saarland.de/tools/unbeast"
    ++ ", " ++
    link "Wring" "http://www.ist.tugraz.at/staff/bloem/wring.html"
    ++ ","
  , "  " ++
    link "structured Slugs"
      ("https://github.com/VerifiableRobotics/slugs/" ++
       "blob/master/doc/input_formats.md#structuredslugs")
    ++ ", and " ++ link "SlugsIn"
    ("https://github.com/VerifiableRobotics/slugs/blob/master/" ++
     "doc/input_formats.md#slugsin") ++ "."
  , ""
  , "* Syntactical analysis of membership in GR(k) for any k (modulo"
  , "  Boolean identities)."
  , ""
  , "* On the fly adjustment of parameters, semantics or targets."
  , ""
  , "* Preprocessing of the resulting LTL formula."
  , ""
  , "* Conversion to negation normal form."
  , ""
  , "* Replacement of derived operators."
  , ""
  , "* Pushing/pulling next, eventually, or globally operators"
  , "  inwards/outwards."
  , ""
  , "* Standard simplifications."
  , switch "\n" ""
  , "## Installation"
  , ""
  , "SyfCo is written in Haskell and can be compiled using the"
  , "Glasgow Haskell Compiler (GHC). To install the tool you can either"
  , "use " ++ link "cabal" "https://www.haskell.org/cabal" ++ " or " ++
    link "stack" "https://docs.haskellstack.org/en/stable/README/" ++
    " (recommended)."
  , "For more information about the purpose of these tools and why you"
  , "should prefer using stack instead of cabal, we recommend reading"
  ,  link "this blog post"
       "https://www.fpcomplete.com/blog/2015/06/why-is-stack-not-cabal" ++
    " by Mathieu Boespflug. "
  , ""
  , "To install the tool with stack use:"
  , ""
  , scb "stack install"
  , ""
  , "Stack then automatically fetches the right compiler version"
  , "and required dependencies. After that it builds and installs"
  , "the package into you local stack path. If you instead prefer"
  , "to build only, use `stack build`."
  , ""
  , "If you insist to use cabal instead, we recommend at least to use"
  , "a sandbox. Initialize the sandbox and configure the project via"
  , ""
  , scb "cabal sandbox init && cabal configure"
  , ""
  , "Then use `cabal build` or `cabal install` to build or install the"
  , "tool."
  , ""
  , "Note that (independent of the chosen build method) building the"
  , "tool will only create the final executable in a hidden sub-folder,"
  , "which might get cumbersome for development or testing local changes."
  , "Hence, for this purpose, you may prefer to use `make`. The makefile"
  , "determines the chosen build method, rebuilds the package, and copies"
  , "the final `syfco` executable to the root directory of the project."
  , ""
  , "If you still encounter any problems, please inform us via the"
  , switch
      ("project bug tracker:\n\n  " ++
       "https://github.com/reactive-systems/syfco/issues\n\n")
      (link "project bug tracker"
            "https://github.com/reactive-systems/syfco/issues"
            ++ ".\n")
  , usage m
  , "## Examples"
  , ""
  , "A number of synthesis benchmarks in TLSF can be found in the"
  , code m "/examples" ++ " directory."
  , ""
  , "## Syfco Library"
  , ""
  , "Syfco is also provided as a Haskell library. In fact, the syfco"
  , "executable is nothing different than a fancy command line interface"
  , "to this library. If you are interested in using the interface, we"
  , "recommend to build and check the interface documentation, which is"
  , "generated by:"
  , ""
  , scb "make haddock"
  , ""
  , "## Editor Support"
  , ""
  , "If you use " ++ link "Emacs" "https://www.gnu.org/software/emacs" ++
    ", you should try our emacs mode (" ++ code m "tlsf-mode.el" ++ "),"
  , "which can be found in the " ++ code m "/misc" ++ " directory."
  , ""
  , "## Adding output formats"
  , ""
  , "If you like to add a new output format, first consider"
  , code m "/Writer/Formats/Example.hs" ++
    ", which contains the most common"
  , "standard constructs and a short tutorial." ]

  where
    scb str = case m of
      Markdown -> "`" ++ str ++ "`"
      _        -> "  " ++ str

    switch s1 s2 = case m of
      Markdown -> s2
      _        -> s1

    link n url = "[" ++ n ++ "](" ++ url ++ ")"

    appendlinks str = case m of
      Markdown -> str
      _        ->
        let (str',ls) = replclct [] [] [] [] 0 0 str
        in str' ++
           "\n--------------------------------------------------\n\n" ++
           concatMap (\(n,s1,s2) -> "[" ++ show n ++ "]" ++
                                    replicate (4 - length (show n)) ' '
                                    ++ s2 ++ "\n") ls

    replclct a b c1 c2 n m xs = case xs of
      []   -> case m of
        0 -> (reverse b, reverse a)
        1 -> (reverse (c1 ++ ('[' : b)), reverse a)
        2 -> (reverse (('(' : ']' : c1) ++ ('[' : b)), reverse a)
        _ -> (reverse (c2 ++ ('(' : ']' : c1) ++ ('[' : b)), reverse a)
      x:xr -> case (x,m) of
        ('[',0) -> replclct a b c1 c2 n 1 xr
        ( _ ,0) -> replclct a (x:b) c1 c2 n 0 xr
        (']',1) -> replclct a b c1 c2 n 2 xr
        ( _ ,1) -> replclct a b (x:c1) c2 n 1 xr
        ('(',2) -> replclct a b c1 c2 n 3 xr
        ( _ ,2) -> replclct a (x:(']':c1) ++ ('[': b)) [] [] n 0 xr
        (')',_) -> replclct ((n,reverse c1, reverse c2):a)
                   ((']' : reverse (show n)) ++ ('[' : ' ' : c1) ++ b)
                   [] [] (n+1) 0 xr
        ( _ ,_) -> replclct a b c1 (x:c2) n 3 xr

-----------------------------------------------------------------------------

code
  :: Mode -> String -> String

code m str = case m of
  Markdown -> "```" ++ concatMap escapePipe str ++ "```"
  _        -> str

  where
    escapePipe '|' = "&#124;"
    escapePipe x   = [x]

-----------------------------------------------------------------------------

codeblock
  :: Mode -> [String] -> String

codeblock m xs = case m of
  Markdown -> "```\n" ++ unlines xs ++ "```\n"
  _        -> unlines $ map ((' ':) . (' ':)) xs

-----------------------------------------------------------------------------

-- | Prints an error to STDERR and then terminates the program.

prError
  :: String -> IO a

prError err = do
  hPutStrLn stderr err
  exitFailure

-----------------------------------------------------------------------------
