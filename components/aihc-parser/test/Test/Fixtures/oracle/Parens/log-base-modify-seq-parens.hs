{- ORACLE_TEST pass -}
{-# LANGUAGE OverloadedStrings #-}

module MonoidAppendParen where

appendToRef ref msg' = ref (<> msg' <> "\n")
