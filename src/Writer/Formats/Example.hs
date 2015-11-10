module Writer.Formats.Example -- <-- CHANGE THIS TO THE NAME OF THE FILE 
       ( writeExample -- <-- CHANGE THIS TO THE NAME OF YOUR WRITER
                      --     (convention: "write" ++ <name of file>
       ) where

---
{- This is an example file, which explains the basics to add a new writer to the tool. Basically,
 - a writer is a function that turns the internal representation of the specification into a
 - string that represents the specification in the desired format. Additionally, a parition file
 - can be created, as it is needed for some formats. In addition to creating this file the following
 - files need to be changed:
 -
 -   Writer/Formats.hs:
 -
 -      Add an internal name for your format here and the string which specifies the format on the
 -      command line. If your format only supports lower case signal names, you can also enable
 -      an automatic conversion here.
 -
 -   Writer.hs:
 -
 -      Add a link here to call your writer procedure.
 -
 -   Info.hs:  (optional)
 -
 -      Add your format with a short description to the '--help' command line output.
 -
 -}
---

import Config
import Simplify

import Data.Error
import Data.Specification

import Writer.Eval
import Writer.Data
import Writer.Utils

---

{- The easies way to create a new format is to take this file as it is and just change the names
 - of the operators below. Your writer then provieds the LTL expression using your operator names
 - and a partition file. Furthermore, it already supports pretty printing.
 -}

opNames
  :: OperatorNames

opNames = OperatorNames
  { opTrue = "<fancy true symbol>" 
  , opFalse = "<fancy false symbol>"
  , opNot = "<fancy not symbol>" 
  , opAnd = "<fancy and symbol>" 
  , opOr = "<fancy or symbol>" 
  , opImplies = "<fancy implication symbol>" 
  , opEquiv = "<fancy equivalence symbol>"
  , opNext = "<fancy next symbol>"
  , opFinally = "<fancy finally symbol>"
  , opGlobally = "<fancy globally symbol>" 
  , opUntil = "<fancy until symbol>" 
  , opRelease = "<fancy release symbol>" 
  , opWeak = "<fancy weak until symbol>"
  }

---

{- The core of each writer is the writing function, which turns the internal format into the 
 - desired format. Thereby consider that the internal representation still allows sets and 
 - functions which are not resolved yet. The writer gets as input the configuration, which
 - contains all settings passed via the command line (see Config.hs for more info), and the
 - specification that contains the internal representation. The writer returns a String,
 - which contains the respecitve content of the main file.
 -
 - Feel free to adapt this function according to you needs. However, the following functions
 - may be useful to simplify you life:
 -
 -   (as,is,gs) <- eval c s
 -
 -     The function gets the configuration and the specification s and returns three lists of
 -     LTL formulas: the assumptions, the invariants, and the guarantees.
 -
 -   fml <- merge as is gs
 -
 -     To combine the lists returned by 'eval' you can use the function merge, which composes
 -     them to one formula in the predefined way. Thereby, the function already takes care
 -     that no unneccessary constructs are introduced, if one or multiple of the lists are
 -     empty.
 -
 -   fml' <- simplify c fml
 -
 -     This function applies all simplifications to the formula fml, that are enabled via
 -     the command line. It is recommended to call this functions to support these
 -     simplifiactions in you format.
 -
 -   str <- pretty mode opNames fml
 -
 -     A simple pretty printer to move from the internal representation to the desired
 -     string representation. Thereby mode selects whether pretty printing or fully
 -     parenthesized printing should be used. It is recommended to use the passed value
 -     stored in 'outputMode c' here. Finally, the opNames structure, which may be define
 -     as above, fixes the names of the operators used here. The function uses the standard
 -     operator precedence for pretty printing.
 -
 - These constructs mostly should suffice to create a writer for simple output formats.
 - However, surely also more advanced stuff can be done (this is haskell ;-). To get further
 - information into this direction it is recommended to first have a look at the other
 - already existing formats. For even more detailed information you may have a look at
 - "Config.hs", "Data.LTL.hs", or "Data.Specification".
 -
 - Finally: Have fun :-)
 - 
 -}

writeExample
  :: Configuration -> Specification -> Either Error String

writeExample c s = do
    (as,is,gs) <- eval c s
    fml0 <- merge as is gs
    fml1 <- simplify c fml0

    return $ pretty (outputMode c) opNames fml1
    
---

