{- ORACLE_TEST
id: single-line
category: expressions
expected: xfail
reason: single line multiline string
-}
{-# LANGUAGE MultilineStrings #-}
module SingleLine where

s :: String
s = """One line"""
