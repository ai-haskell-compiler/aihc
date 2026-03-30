{- ORACLE_TEST xfail record wildcard in pattern -}
{-# LANGUAGE RecordWildCards #-}
module Pattern where

data Point = Point { x, y :: Int }

f Point{..} = x + y