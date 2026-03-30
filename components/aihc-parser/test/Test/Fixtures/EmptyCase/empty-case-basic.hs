{- ORACLE_TEST
id: empty-case-basic
category: expressions
expected: pass
reason: parser now supports empty case alternatives
-}
{-# LANGUAGE EmptyCase #-}

module EmptyCaseBasic where

data Void

absurd :: Void -> a
absurd v = case v of {}
