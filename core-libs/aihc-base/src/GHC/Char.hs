{-# LANGUAGE MagicHash #-}

module GHC.Char
  ( chr,
    ord,
  )
where

import GHC.Int (Int (..))
import GHC.Prim.Unicode (charToInt#, intToChar#)

chr :: Int -> Char
chr (I# value) = intToChar# value

ord :: Char -> Int
ord value = I# (charToInt# value)
