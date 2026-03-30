{- ORACLE_TEST
id: thq_name_quote
category: expressions
expected: xfail
reason: TemplateHaskellQuotes 'f syntax
-}
{-# LANGUAGE TemplateHaskellQuotes #-}
module THQ_Name_Quote where

fName = 'map
trueName = 'True
fNameSpace = ' map
