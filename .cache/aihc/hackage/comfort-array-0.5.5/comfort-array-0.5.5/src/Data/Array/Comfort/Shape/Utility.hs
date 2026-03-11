module Data.Array.Comfort.Shape.Utility where

import Text.Printf (printf)


messageIndexFromOffset :: String -> Int -> String
messageIndexFromOffset name k =
   printf "indexFromOffset (%s): index %d out of range" name k

errorIndexFromOffset :: String -> Int -> a
errorIndexFromOffset name = error . messageIndexFromOffset name


isRight :: Either a b -> Bool
isRight = either (const False) (const True)
