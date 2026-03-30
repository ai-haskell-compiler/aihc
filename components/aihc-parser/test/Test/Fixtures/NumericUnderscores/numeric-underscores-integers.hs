{- ORACLE_TEST
id: numeric-underscores-integers
category: literals
expected: pass
reason: parser now accepts NumericUnderscores integer literals
-}
{-# LANGUAGE NumericUnderscores #-}

module NumericUnderscoresIntegers where

million :: Integer
million = 1_000_000

hexWord :: Integer
hexWord = 0xDEAD_BEEF
