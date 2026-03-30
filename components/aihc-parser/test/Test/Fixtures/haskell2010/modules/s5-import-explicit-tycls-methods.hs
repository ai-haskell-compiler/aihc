{- ORACLE_TEST
id: modules-s5-import-explicit-tycls-methods
category: modules
expected: pass
reason: parser now supports section 5 explicit class method imports
-}
module S5ImportExplicitTyClsMethods where
import Prelude (Ord(compare))
x = compare 1 2
