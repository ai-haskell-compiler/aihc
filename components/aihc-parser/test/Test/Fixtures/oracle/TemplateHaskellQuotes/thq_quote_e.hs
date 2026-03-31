{- ORACLE_TEST pass -}
{-# LANGUAGE TemplateHaskellQuotes #-}
module THQ_Quote_E where

f = [| 1 + 2 |]
g = [e| 1 + 2 |]