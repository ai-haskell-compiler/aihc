{- ORACLE_TEST
id: hex-float-literals-basic
category: literals
expected: pass
reason: parser now supports hexadecimal floating-point literals with binary exponent
-}
{-# LANGUAGE HexFloatLiterals #-}

module HexFloatLiteralsBasic where

unit :: Double
unit = 0x1p0

shifted :: Double
shifted = 0x1p4
