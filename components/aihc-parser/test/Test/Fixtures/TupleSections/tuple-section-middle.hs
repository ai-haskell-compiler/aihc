{- ORACLE_TEST
id: tuple-section-middle
category: expressions
expected: pass
reason: parser now supports tuple sections
-}
{-# LANGUAGE TupleSections #-}

module TupleSectionMiddle where

fillMiddle :: Int -> (Int, Int, Int)
fillMiddle = (,2,)
