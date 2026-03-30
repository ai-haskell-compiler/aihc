{- ORACLE_TEST xfail basic multiline string -}
{-# LANGUAGE MultilineStrings #-}
module Basic where

s :: String
s = """
    Hello,
    World!
    """