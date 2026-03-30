{- ORACLE_TEST
id: basic
category: expressions
expected: xfail
reason: basic multiline string
-}
{-# LANGUAGE MultilineStrings #-}
module Basic where

s :: String
s = """
    Hello,
    World!
    """
