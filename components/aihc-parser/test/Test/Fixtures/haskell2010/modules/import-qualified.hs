{- ORACLE_TEST
id: modules-import-qualified
category: modules
expected: pass
reason: parser now supports qualified imports with aliases
-}
module IQ where
import qualified Data.List as L
x = L.length []
