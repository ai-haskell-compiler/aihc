{- ORACLE_TEST
id: modules-import-hiding
category: modules
expected: pass
reason: parser now supports hiding import lists
-}
module IH where
import Data.List hiding (map)
x = foldr (+) 0 [1,2,3]
