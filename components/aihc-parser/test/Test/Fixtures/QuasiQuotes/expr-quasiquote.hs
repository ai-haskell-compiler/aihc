{- ORACLE_TEST
id: expr-quasiquote
category: expressions
expected: xfail
reason: parser intentionally disabled
-}
{-# LANGUAGE QuasiQuotes #-}
module ExprQuasiQuote where

x = [sql|select * from users where id = 1|]
