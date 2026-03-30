{- ORACLE_TEST
id: viewpatterns-funarg
category: patterns
expected: pass
reason: parser now supports view patterns in function arguments
-}
{-# LANGUAGE ViewPatterns #-}

module ViewPatternsFunArg where

project :: a -> a
project x = x

useView :: a -> a
useView (project -> x) = x
