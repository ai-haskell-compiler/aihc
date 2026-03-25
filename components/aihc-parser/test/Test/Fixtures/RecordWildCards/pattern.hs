{-# LANGUAGE RecordWildCards #-}
module Pattern where

data Point = Point { x, y :: Int }

f Point{..} = x + y
