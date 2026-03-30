{- ORACLE_TEST
id: numeric-underscores-pattern
category: patterns
expected: pass
reason: parser now accepts NumericUnderscores literals in patterns
-}
{-# LANGUAGE NumericUnderscores #-}

module NumericUnderscoresPattern where

classify :: Integer -> String
classify n = case n of
  1_024 -> "kibi"
  65_536 -> "mebi"
  _ -> "other"
