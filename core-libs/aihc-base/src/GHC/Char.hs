{-# LANGUAGE MagicHash #-}

module GHC.Char
  ( chr,
    eqChar,
    neChar,
    ord,
    unsafeChr,
  )
where

import GHC.Int (Int (..))
import GHC.Internal.Char (Char (C#))
import GHC.Prim (chr#, ord#, (/=#), (==#))
import GHC.Types (Bool, isTrue#)

chr :: Int -> Char
chr (I# value) = C# (chr# value)

-- | Convert a code point to a character without a range check.
unsafeChr :: Int -> Char
unsafeChr (I# value) = C# (chr# value)

ord :: Char -> Int
ord (C# value) = I# (ord# value)

-- | Compare two characters for equality.
eqChar :: Char -> Char -> Bool
eqChar (C# left) (C# right) = isTrue# (ord# left ==# ord# right)

-- | Compare two characters for inequality.
neChar :: Char -> Char -> Bool
neChar (C# left) (C# right) = isTrue# (ord# left /=# ord# right)
