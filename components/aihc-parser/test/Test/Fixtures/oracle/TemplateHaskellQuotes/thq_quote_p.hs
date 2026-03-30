{- ORACLE_TEST xfail TemplateHaskellQuotes [p|t|] syntax -}
{-# LANGUAGE TemplateHaskellQuotes #-}
module THQ_Quote_P where

pat = [p| Just x |]