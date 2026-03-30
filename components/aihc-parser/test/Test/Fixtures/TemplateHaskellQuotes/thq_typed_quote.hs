{- ORACLE_TEST
id: thq_typed_quote
category: expressions
expected: xfail
reason: TemplateHaskellQuotes [||e||] syntax
-}
{-# LANGUAGE TemplateHaskellQuotes #-}
module THQ_Typed_Quote where

tq = [|| 1 + 2 ||]
tqe = [e|| 1 + 2 ||]
