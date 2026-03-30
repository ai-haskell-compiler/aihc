{- ORACLE_TEST
id: empty-case-multiline-type-param
category: expressions
expected: pass
reason: parser now supports multiline data declaration heads
-}
{-# LANGUAGE EmptyCase #-}

module EmptyCaseMultilineTypeParam where

data Test
 a

x = 1
