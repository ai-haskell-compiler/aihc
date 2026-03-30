{- ORACLE_TEST
id: modules-s5-import-explicit-tycls-all
category: modules
expected: pass
reason: parser now supports section 5 explicit class imports with wildcard
-}
module S5ImportExplicitTyClsAll where
import Prelude (Ord(..))
x = EQ
