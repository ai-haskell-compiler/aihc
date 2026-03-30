{- ORACLE_TEST
id: tuple-section-left
category: expressions
expected: pass
reason: parser now supports tuple sections
-}
{-# LANGUAGE TupleSections #-}

module TupleSectionLeft where

pairWithOne :: Int -> (Int, Int)
pairWithOne = (1,)
