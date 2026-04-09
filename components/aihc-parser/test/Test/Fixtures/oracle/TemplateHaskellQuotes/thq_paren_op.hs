{- ORACLE_TEST xfail TH quote tick with parenthesized operator -}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE TemplateHaskell #-}

module THQuoteTickOperator where

f = ''(:>)
