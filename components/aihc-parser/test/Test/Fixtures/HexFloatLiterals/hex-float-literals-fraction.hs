{- ORACLE_TEST
id: hex-float-literals-fraction
category: literals
expected: pass
reason: parser now supports hexadecimal floating-point literals with fractional mantissas
-}
{-# LANGUAGE HexFloatLiterals #-}

module HexFloatLiteralsFraction where

half :: Double
half = 0x1p-1

fractional :: Double
fractional = 0x1.8p1
