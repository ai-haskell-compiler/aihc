{- ORACLE_TEST
id: manual-linebreaks
category: expressions
expected: xfail
reason: manual linebreaks \&
-}
{-# LANGUAGE MultilineStrings #-}
module ManualLinebreaks where

s :: String
s = """
    Hello \&
    World
    """
