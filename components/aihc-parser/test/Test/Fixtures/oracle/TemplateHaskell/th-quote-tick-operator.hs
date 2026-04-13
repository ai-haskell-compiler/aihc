{- ORACLE_TEST pass -}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE DataKinds #-}

module THQuoteTickOperator where

type (:>) a b = '(a, b)

test = ''(:>)
