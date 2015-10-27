module Writer.Data where

---

data WriteContents =
  WriteContents
  { mainFile :: String
  , partitionFile :: Maybe String
  }

---

data WriteMode =
    Pretty
  | Fully
  deriving (Eq, Show)

---

data OperatorNames =
  OperatorNames
  { opTrue :: String
  , opFalse :: String
  , opNot :: String
  , opAnd :: String
  , opOr :: String
  , opImplies :: String
  , opEquiv :: String
  , opNext :: String
  , opFinally :: String
  , opGlobally :: String
  , opUntil :: String
  , opRelease :: String
  , opWeak :: String
  }

---  
