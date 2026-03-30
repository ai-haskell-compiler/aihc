{- ORACLE_TEST
id: indentation
category: expressions
expected: xfail
reason: indentation stripping
-}
{-# LANGUAGE MultilineStrings #-}
module Indentation where

s :: String
s = """
      This is indented
    by two spaces.
    """
