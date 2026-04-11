{- ORACLE_TEST xfail reason="TemplateHaskell quote tick for type operator not handled" -}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE DataKinds #-}

module THQuoteTickOperator where

type (:>) a b = '(a, b)

test = ''(:>)
