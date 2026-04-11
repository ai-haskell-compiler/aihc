{- ORACLE_TEST xfail reason="TemplateHaskell type quote with parenthesized arrow ''(->) not parsed" -}
{-# LANGUAGE TemplateHaskell #-}

module THQuotePromotedArrow where

f = ''(->)
