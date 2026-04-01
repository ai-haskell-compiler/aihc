{- ORACLE_TEST pass -}
{-# LANGUAGE RecordWildCards #-}
module Mixed where

data Point = Point { x, y :: Int }

f Point{x, ..} = x + y