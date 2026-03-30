{- ORACLE_TEST
id: type-quasiquote
category: types
expected: pass
reason: parser now supports type quasiquotes
-}
{-# LANGUAGE QuasiQuotes #-}
module TypeQuasiQuote where

f :: [sql|INT|] -> Int
f _ = 0
