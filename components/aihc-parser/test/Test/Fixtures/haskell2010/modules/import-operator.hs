{- ORACLE_TEST
id: modules-import-operator
category: modules
expected: pass
reason: parser now supports parenthesized type operators with wildcard in import lists
-}
{-# LANGUAGE TypeOperators #-}
module ImportOperator where
import Data.Type.Equality ((:~~:)(..))
