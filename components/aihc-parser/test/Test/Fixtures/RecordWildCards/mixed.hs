{- ORACLE_TEST
id: mixed
category: patterns
expected: xfail
reason: mixed record fields and wildcard
-}
{-# LANGUAGE RecordWildCards #-}
module Mixed where

data Point = Point { x, y :: Int }

f Point{x, ..} = x + y
