{- ORACLE_TEST xfail manual linebreaks \& -}
{-# LANGUAGE MultilineStrings #-}
module ManualLinebreaks where

s :: String
s = """
    Hello \&
    World
    """