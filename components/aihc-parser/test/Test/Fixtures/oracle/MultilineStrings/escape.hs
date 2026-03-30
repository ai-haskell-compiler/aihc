{- ORACLE_TEST xfail escape sequences -}
{-# LANGUAGE MultilineStrings #-}
module Escape where

s :: String
s = """
    Line 1\nLine 2
    """