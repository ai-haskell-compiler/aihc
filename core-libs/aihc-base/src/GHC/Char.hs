{-# LANGUAGE MagicHash #-}

module GHC.Char
  ( chr,
    ord,
  )
where

import GHC.Int (Int (..))
import GHC.Prim (intToChar#, ord#)
import Prelude (Char (C#))

chr :: Int -> Char
chr (I# value) = C# (intToChar# value)

ord :: Char -> Int
ord (C# value) = I# (ord# value)
