{- ORACLE_TEST xfail record wildcard in construction -}
{-# LANGUAGE RecordWildCards #-}
module Construction where

data Point = Point { x, y :: Int }

f x y = Point{..}