{- ORACLE_TEST
id: viewpatterns-casealt
category: patterns
expected: pass
reason: parser now supports view patterns in case alternatives
-}
{-# LANGUAGE ViewPatterns #-}

module ViewPatternsCaseAlt where

project :: a -> a
project x = x

useCase :: a -> a
useCase input =
  case input of
    (project -> x) -> x
