{- ORACLE_TEST xfail mixed record fields and wildcard -}
{-# LANGUAGE RecordWildCards #-}
module Mixed where

data Point = Point { x, y :: Int }

f Point{x, ..} = x + y