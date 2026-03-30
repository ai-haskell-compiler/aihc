{- ORACLE_TEST
id: tuple-section-nested
category: expressions
expected: pass
reason: parser now supports tuple sections
-}
{-# LANGUAGE TupleSections #-}

module TupleSectionNested where

nested :: Int -> ((Int, Int), Int)
nested = ((,3),)
