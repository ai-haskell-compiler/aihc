{- ORACLE_TEST xfail TemplateHaskellQuotes [t|t|] syntax -}
{-# LANGUAGE TemplateHaskellQuotes #-}
module THQ_Quote_T where

typ = [t| Int -> Int |]