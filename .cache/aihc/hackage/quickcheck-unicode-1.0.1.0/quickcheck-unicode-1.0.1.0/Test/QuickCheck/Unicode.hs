{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module      : Test.QuickCheck.Unicode
-- Copyright   : (c) 2014-2017 Bryan O'Sullivan
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : stable
-- Portability : portable
--
-- QuickCheck Generator and shrink functions for testing software that
-- uses Unicode data.
--
-- The default 'Arbitrary' instance for the 'Char' type intentionally
-- generates only ASCII values.  This can lead to a false sense of
-- security in cases where Unicode compliance is required, as
-- encodings that span multiple bytes or code units will simply not be
-- exercised at all.
--
-- This module deliberately avoids using the @text@ and @bytestring@
-- packages to avoid pulling in extra dependencies.

module Test.QuickCheck.Unicode
    (
    -- * Newtype wrapper for convenience
      Unicode(fromUnicode)

    -- * Generators
    , char
    , string
    , string1

    -- ** Helpers
    , list
    , list1

    -- ** Basic generators
    , planes
    , ascii
    , plane0
    , plane1
    , plane2
    , plane14

    -- * Predicates
    , reserved

    -- * Shrinking functions
    , shrinkChar
    ) where

import Control.Applicative ((<$>))
import Data.Bits ((.&.))
import Data.Char (chr, ord)
import Test.QuickCheck hiding ((.&.))

-- | A wrapper for 'Char' and 'String', for which the 'Arbitrary'
-- instance generates full-Unicode characters.
newtype Unicode a = Unicode { fromUnicode :: a }
                  deriving (Eq, Ord, Show, Read)

instance Arbitrary (Unicode Char) where
    arbitrary = Unicode <$> char
    shrink    = map Unicode . shrinkChar . fromUnicode

instance Arbitrary (Unicode [Char]) where
    arbitrary = Unicode <$> string
    shrink    = map Unicode . shrinkList shrinkChar . fromUnicode

-- | Generate a Unicode code point.  This has a much larger range than
-- the default 'Arbitrary' instance for 'Char'.
char :: Gen Char
char = chr `fmap` excluding reserved (frequency planes)

-- | Generate a list of Unicode code points.
string :: Gen String
string = list char

-- | Generate a non-empty list of Unicode code points.
string1 :: Gen String
string1 = list char

-- | Generate a list of values.
list :: Gen a -> Gen [a]
list gen = listN 0 gen

-- | Generate a non-empty list of values.
list1 :: Gen a -> Gen [a]
list1 gen = listN 1 gen

-- | Generate a list of at least /n/ values.
listN :: Int -> Gen a -> Gen [a]
listN m gen =
  sized $ \n ->
    do k <- choose (m,n)
       vectorOf k gen

-- | Shrink a Unicode code point.
shrinkChar :: Char -> [Char]
shrinkChar = map chr . filter (not . reserved) . shrinkIntegral . ord

excluding :: (a -> Bool) -> Gen a -> Gen a
excluding bad gen = loop
  where
    loop = do
      x <- gen
      if bad x
        then loop
        else return x

-- | Indicate whether a code point is reserved.
reserved :: Int -> Bool
reserved = anyOf [(<0), (>0x10FFFF), lowSurrogate, highSurrogate, nonCharacter]
  where
    anyOf fs xs     = or (map ($ xs) fs)
    lowSurrogate c  = c >= 0xDC00 && c <= 0xDFFF
    highSurrogate c = c >= 0xD800 && c <= 0xDBFF
    nonCharacter c  = masked == 0xFFFE || masked == 0xFFFF
      where masked  = c .&. 0xFFFF

-- | A weighted list of generators that favours ASCII characters,
-- followed by planes 0 and 1.
planes :: [(Int, Gen Int)]
planes = [(60, ascii),
          (14, plane0),
          (14, plane1),
          (6,  plane2),
          (6,  plane14)]

-- ASCII.
ascii :: Gen Int
ascii = choose (0,0x7F)

-- | Basic Multilingual Plane.
plane0 :: Gen Int
plane0 = choose (0xF0, 0xFFFF)

-- | Supplementary Multilingual Plane.
plane1 :: Gen Int
plane1 = oneof [ choose (0x10000, 0x10FFF)
               , choose (0x11000, 0x11FFF)
               , choose (0x12000, 0x12FFF)
               , choose (0x13000, 0x13FFF)
               , choose (0x1D000, 0x1DFFF)
               , choose (0x1F000, 0x1FFFF)
               ]

-- | Supplementary Ideographic Plane.
plane2 :: Gen Int
plane2 = oneof [ choose (0x20000, 0x20FFF)
               , choose (0x21000, 0x21FFF)
               , choose (0x22000, 0x22FFF)
               , choose (0x23000, 0x23FFF)
               , choose (0x24000, 0x24FFF)
               , choose (0x25000, 0x25FFF)
               , choose (0x26000, 0x26FFF)
               , choose (0x27000, 0x27FFF)
               , choose (0x28000, 0x28FFF)
               , choose (0x29000, 0x29FFF)
               , choose (0x2A000, 0x2AFFF)
               , choose (0x2B000, 0x2BFFF)
               , choose (0x2F000, 0x2FFFF)
               ]

-- | Supplementary Special-Purpose Plane.
plane14 :: Gen Int
plane14 = choose (0xE0000, 0xE0FFF)
