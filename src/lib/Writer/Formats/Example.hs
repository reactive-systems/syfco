module Writer.Formats.Example where

--- ^^^ CHANGE THIS TO THE NAME OF THE FILE / YOUR FORMAT

-----------------------------------------------------------------------------

{- This is an example file, which explains the basics to add a new
 - writer to the tool. Basically, a writer is a function that turns the
 - internal representation of the specification into a string that
 - represents the specification in the desired format. Additionally, a
 - parition file can be created, as it is needed for some formats. In
 - addition to creating this file the following files need to be changed:
 -
 -   Writer/Formats.hs:
 -
 -      Add an internal name for your format here and the string which
 -      specifies the format on the command line. If your format only
 -      supports lower case signal names, you can also enable an
 -      automatic conversion here. Finally, if you use an operator
 -      config, link it here.
 -
 -   Writer.hs:
 -
 -      Add a link here to call your writer procedure.
 -
 -   Info.hs:  (optional)
 -
 -      Add your format with a short description to the '--help' command
 -      line output.
 -
 -}

import Config
import Simplify

import Data.Error
import Data.Specification

import Writer.Eval
import Writer.Data
import Writer.Utils

-----------------------------------------------------------------------------

{- The easies way to create a new format is to take this file as it is
 - and just change the names of the operators below, their precedence
 - and their associativity. Thereby, for the precedence a lower value
 - means higher precedence, where two operators with the same precedence
 - value are threated equally. The associativity is either: 'AssocLeft'
 - or 'AssocRight'. If an operator can be derived from the others, then
 - it can be disabled by using 'UnaryOpUnsupported' or
 - 'binaryOpUnsupported', respectively.
 -
 - The writer then provieds the LTL expression using your operator names
 - and also already supports pretty printing.
 -}

opConfig
  :: OperatorConfig

opConfig = OperatorConfig
  { tTrue        = "<fancy true symbol>"
  , fFalse       = "<fancy false symbol>"
  , opNot        = UnaryOp  "<fancy not symbol>"         1
  , opAnd        = BinaryOp "<fancy and symbol>"         6 AssocLeft
  , opOr         = BinaryOp "<fancy or symbol>"          7 AssocLeft
  , opImplies    = BinaryOp "<fancy implication symbol>" 8 AssocRight
  , opEquiv      = BinaryOp "<fancy equivalence symbol>" 8 AssocLeft
  , opNext       = UnaryOp  "<fancy next symbol>"        2
  , opStrongNext = UnaryOp  "<fancy next symbol>"        2
  , opFinally    = UnaryOp  "<fancy finally symbol>"     3
  , opGlobally   = UnaryOp  "<fancy globally symbol>"    4
  , opUntil      = BinaryOp "<fancy until symbol>"       5 AssocRight
  , opRelease    = BinaryOp "<fancy release symbol>"     9 AssocLeft
  , opWeak       = BinaryOpUnsupported
  }

-----------------------------------------------------------------------------

{- The core of each writer is the writing function, which turns the
 - internal format into the desired format. Thereby consider that the
 - internal representation still allows sets and functions which are not
 - resolved yet. The writer gets as input the configuration, which
 - contains all settings passed via the command line (see Config.hs for
 - more info), and the specification that contains the internal
 - representation. The writer returns a String, which contains the
 - respecitve content of the main file.
 -
 - Feel free to adapt this function according to you needs. However,
 - the following functions may be useful to simplify you life:
 -
 -   (es,ss,rs,as,is,gs) <- eval c s
 -
 -     The function gets the configuration and the specification s and
 -     returns six lists of LTL formulas: the initial configuration of
 -     the environment, the initial configuration of the system, the
 -     requirements, the assumptions, the invariants, and the guarantees.
 -
 -   fml <- merge es ss rs as is gs
 -
 -     To combine the lists returned by 'eval' you can use the function
 -     merge, which composes them to one formula in the predefined way.
 -     Thereby, the function already takes care that no unneccessary
 -     constructs are introduced, in case one or multiple of the lists
 -     are empty.
 -
 -   fml' <- simplify (adjust c opConfig) fml
 -
 -     This function applies all simplifications to the formula fml,
 -     that are enabled via the command line. It is recommended to call
 -     this functions to support these simplifiactions in you format.
 -     If you have unsupported operators you have to adjust the
 -     configuration such that they can be removed by 'simplify'.
 -
 -   str <- printformula opConfig mode fml
 -
 -     A simple printer (supporting pretty printing) to move from the
 -     internal representation to the desired string representation.
 -     Thereby mode selects whether pretty printing or fully
 -     parenthesized printing should be used. It is recommended to use
 -     the passed value stored in 'outputMode c' here. Finally, the
 -     opConfig structure, which may be define as above, fixes the
 -     names of the operators used here. The function uses the standard
 -     operator precedence for pretty printing.
 -
 - These constructs mostly should suffice to create a writer for simple
 - output formats. However, surely also more advanced stuff can be done
 - (this is haskell ;-). To get further information into this direction
 - it is recommended to first have a look at the other already existing
 - formats. For even more detailed information you may have a look at
 - "Config.hs", "Data.LTL.hs", or "Data.Specification".
 -
 - Finally: Have fun :-)
 -
 -}

writeFormat
  :: Configuration -> Specification -> Either Error String

writeFormat c s = do
    (es,ss,rs,as,is,gs) <- eval c s
    fml0 <- merge es ss rs as is gs
    fml1 <- simplify (adjust c opConfig) fml0

    printFormula opConfig (outputMode c) (quoteMode c) fml1

-----------------------------------------------------------------------------
