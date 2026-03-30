{- ORACLE_TEST
id: thq_quote_d
category: declarations
expected: xfail
reason: TemplateHaskellQuotes [d|t|] syntax
-}
{-# LANGUAGE TemplateHaskellQuotes #-}
module THQ_Quote_D where

decl = [d| f x = x |]
