{- ORACLE_TEST
id: viewpatterns-patbind
category: patterns
expected: pass
reason: parser now supports pattern binding declarations with view patterns
-}
{-# LANGUAGE ViewPatterns #-}

module ViewPatternsPatBind where

project :: a -> a
project x = x

bound :: a
(project -> bound) = project value
  where
    value = error "fixture"
