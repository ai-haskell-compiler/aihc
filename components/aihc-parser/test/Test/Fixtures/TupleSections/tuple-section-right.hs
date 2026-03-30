{- ORACLE_TEST
id: tuple-section-right
category: expressions
expected: pass
reason: parser now supports tuple sections
-}
{-# LANGUAGE TupleSections #-}

module TupleSectionRight where

pairWithLast :: Int -> (Int, Int)
pairWithLast = (,1)
