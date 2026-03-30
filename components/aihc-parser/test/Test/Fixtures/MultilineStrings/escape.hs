{- ORACLE_TEST
id: escape
category: expressions
expected: xfail
reason: escape sequences
-}
{-# LANGUAGE MultilineStrings #-}
module Escape where

s :: String
s = """
    Line 1\nLine 2
    """
