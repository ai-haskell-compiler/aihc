{- ORACLE_TEST xfail log-base-style section-appends keep rhs grouped when parenthesized by parser -}
{-# LANGUAGE OverloadedStrings #-}

module MonoidAppendParen where

appendToRef ref msg' = ref (<> msg' <> "\n")
