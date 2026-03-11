{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- Module      : Data.Text.Manipulate.Internal.Types
-- Copyright   : (c) 2014-2020 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Data.Text.Manipulate.Internal.Types where

import Control.Monad
import qualified Data.Char as Char
import Data.Text.Lazy.Builder (Builder, singleton)
import GHC.Base

-- | Returns 'True' for any boundary or uppercase character.
isWordBoundary :: Char -> Bool
isWordBoundary c = Char.isUpper c || isBoundary c

-- | Returns 'True' for any boundary character.
isBoundary :: Char -> Bool
isBoundary = not . Char.isAlphaNum

ordinal :: Integral a => a -> Builder
ordinal (toInteger -> n) = decimal n <> suf
  where
    suf
      | x `elem` [11 .. 13] = "th"
      | y == 1 = "st"
      | y == 2 = "nd"
      | y == 3 = "rd"
      | otherwise = "th"

    (flip mod 100 -> x, flip mod 10 -> y) = join (,) (abs n)
{-# NOINLINE [0] ordinal #-}

decimal :: Integral a => a -> Builder
{-# SPECIALIZE decimal :: Int -> Builder #-}
decimal i
  | i < 0 = singleton '-' <> go (- i)
  | otherwise = go i
  where
    go n
      | n < 10 = digit n
      | otherwise = go (n `quot` 10) <> digit (n `rem` 10)
{-# NOINLINE [0] decimal #-}

digit :: Integral a => a -> Builder
digit n = singleton $! i2d (fromIntegral n)
{-# INLINE digit #-}

i2d :: Int -> Char
i2d (I# i#) = C# (chr# (ord# '0'# +# i#))
{-# INLINE i2d #-}
