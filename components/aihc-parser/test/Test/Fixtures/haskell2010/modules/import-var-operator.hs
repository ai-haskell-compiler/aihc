{- ORACLE_TEST
id: modules-import-var-operator
category: modules
expected: pass
reason: parser now supports parenthesized variable operators in import lists
-}
import           Control.Applicative       ((<$>))
