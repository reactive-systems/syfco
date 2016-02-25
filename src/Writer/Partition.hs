module Writer.Partition where

---

partitionLtl
  :: [String] -> [String] -> String

partitionLtl is os =
  ".inputs" ++ concatMap (' ' :) is ++ "\n" ++
  ".outputs" ++ concatMap (' ' :) os ++ "\n"
  
---  
  
