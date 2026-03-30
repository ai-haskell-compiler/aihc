{- ORACLE_TEST
id: pat-quasiquote
category: patterns
expected: pass
reason: parser now supports pattern quasiquotes
-}
{-# LANGUAGE QuasiQuotes #-}
module PatQuasiQuote where

isMatch [sql|user:{id}|] = True
isMatch _ = False
