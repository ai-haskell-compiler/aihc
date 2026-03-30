{- ORACLE_TEST
id: pattern
category: patterns
expected: xfail
reason: record wildcard in pattern
-}
{-# LANGUAGE RecordWildCards #-}
module Pattern where

data Point = Point { x, y :: Int }

f Point{..} = x + y
