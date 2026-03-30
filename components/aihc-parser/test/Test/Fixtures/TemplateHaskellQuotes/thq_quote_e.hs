{- ORACLE_TEST
id: thq_quote_e
category: expressions
expected: xfail
reason: TemplateHaskellQuotes [|e|] syntax
-}
{-# LANGUAGE TemplateHaskellQuotes #-}
module THQ_Quote_E where

f = [| 1 + 2 |]
g = [e| 1 + 2 |]
