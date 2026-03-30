{- ORACLE_TEST
id: thq_type_quote
category: types
expected: xfail
reason: TemplateHaskellQuotes ''T syntax
-}
{-# LANGUAGE TemplateHaskellQuotes #-}
module THQ_Type_Quote where

tName = ''Int
cName = ''Eq
tNameSpace = ''Int