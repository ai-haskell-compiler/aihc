{- ORACLE_TEST
id: modules-deprecated-roundtrip
category: modules
expected: pass
reason: parser supports roundtrip of module deprecated pragmas
-}
module DeprecatedRoundtrip
  {-# DEPRECATED "Use SomethingElse instead" #-}
  where

y = 2
