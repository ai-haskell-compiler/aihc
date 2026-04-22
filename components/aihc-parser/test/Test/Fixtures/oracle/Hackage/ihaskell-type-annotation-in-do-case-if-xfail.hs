{- ORACLE_TEST pass -}
module IHaskellTypeAnnotationInDoCaseIf where

f flag y = do
  case y of
    Right b ->
      if flag
        then b :: [Int]
        else b
