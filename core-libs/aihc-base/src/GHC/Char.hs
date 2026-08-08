{-# LANGUAGE MagicHash #-}

module GHC.Char
  ( chr,
    ord,
  )
where

import GHC.Int (Int (..))
import GHC.Internal.Char (Char (C#))
import GHC.Prim (chr#, ord#)

chr :: Int -> Char
chr (I# value) = C# (chr# value)

ord :: Char -> Int
ord (C# value) = I# (ord# value)
