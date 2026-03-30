{- ORACLE_TEST
id: modules-warning-roundtrip
category: modules
expected: pass
reason: parser supports roundtrip of module warning pragmas
-}
module WarningRoundtrip
  {-# WARNING "This is a test warning" #-}
  where

x = 1
