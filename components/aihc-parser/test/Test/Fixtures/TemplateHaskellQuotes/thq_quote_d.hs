{- ORACLE_TEST xfail TemplateHaskellQuotes [d|t|] syntax -}
{-# LANGUAGE TemplateHaskellQuotes #-}
module THQ_Quote_D where

decl = [d| f x = x |]