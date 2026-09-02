{-# LANGUAGE MagicHash #-}

module GHC.Char
  ( chr,
    ord,
    unsafeChr,
  )
where

import GHC.Int (Int (..))
import GHC.Internal.Char (Char (C#))
import GHC.Prim (chr#, ord#)

chr :: Int -> Char
chr (I# value) = C# (chr# value)

-- | Convert a code point to a character without a range check.
unsafeChr :: Int -> Char
unsafeChr (I# value) = C# (chr# value)

ord :: Char -> Int
ord (C# value) = I# (ord# value)
