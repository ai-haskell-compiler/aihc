{- ORACLE_TEST
id: thq_quote_p
category: patterns
expected: xfail
reason: TemplateHaskellQuotes [p|t|] syntax
-}
{-# LANGUAGE TemplateHaskellQuotes #-}
module THQ_Quote_P where

pat = [p| Just x |]
