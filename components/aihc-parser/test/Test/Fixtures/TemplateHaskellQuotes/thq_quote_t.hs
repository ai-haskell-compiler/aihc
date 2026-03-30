{- ORACLE_TEST
id: thq_quote_t
category: types
expected: xfail
reason: TemplateHaskellQuotes [t|t|] syntax
-}
{-# LANGUAGE TemplateHaskellQuotes #-}
module THQ_Quote_T where

typ = [t| Int -> Int |]
