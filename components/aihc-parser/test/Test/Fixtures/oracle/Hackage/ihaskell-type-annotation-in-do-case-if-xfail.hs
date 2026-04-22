{- ORACLE_TEST xfail parser fails on type annotation in then-branch of if inside do-case alternative -}
module IHaskellTypeAnnotationInDoCaseIf where

f flag y = do
  case y of
    Right b ->
      if flag
        then b :: [Int]
        else b
