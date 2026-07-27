{-# LANGUAGE MagicHash #-}

module GHC.Char
  ( chr,
    ord,
  )
where

import GHC.Int (Int (..))
import GHC.Prim (chr#, ord#)
import Prelude (Char (C#))

chr :: Int -> Char
chr (I# value) = C# (chr# value)

ord :: Char -> Int
ord (C# value) = I# (ord# value)
