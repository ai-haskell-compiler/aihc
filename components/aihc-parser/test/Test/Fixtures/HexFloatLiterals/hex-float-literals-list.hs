{- ORACLE_TEST
id: hex-float-literals-list
category: literals
expected: pass
reason: parser now supports hexadecimal floating-point literals in list expressions
-}
{-# LANGUAGE HexFloatLiterals #-}

module HexFloatLiteralsList where

constants :: [Double]
constants = [0X1.0p0, 0x1.2p3, -0x1p2, 0xAp-1]
