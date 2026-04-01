{- ORACLE_TEST pass -}
{-# LANGUAGE RecordWildCards #-}
module Construction where

data Point = Point { x, y :: Int }

f x y = Point{..}