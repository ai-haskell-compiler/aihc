{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Convenience wrappers and utilities for byte strings.
module Zenacy.HTML.Internal.BS
  ( BS
  -- * Word functions.
  , bsEmpty
  , bsOnly
  , bsLen
  , bsPack
  , bsUnpack
  , bsConcat
  , bsIndex
  , bsElemIndex
  , bsLower
  , bsPrefixCI
  , bsPart
  , bsLast
  , bsTake
  , bsDrop
  , bsUncons
  -- * Character functions.
  , bcPack
  , bcUnpack
  ) where

import Zenacy.HTML.Internal.Char
import Data.ByteString
  ( ByteString
  )
import qualified Data.ByteString as S
  ( concat
  , drop
  , elemIndex
  , empty
  , isPrefixOf
  , index
  , last
  , length
  , map
  , pack
  , singleton
  , take
  , uncons
  , unpack
  )
import qualified Data.ByteString.Char8 as C
  ( pack
  , unpack
  )
import Data.Word
  ( Word8
  )

-- | A type abbreviation for a byte string.
type BS = ByteString

-- | An empty byte string.
bsEmpty :: BS
bsEmpty = S.empty
{-# INLINE bsEmpty #-}

-- | A byte string with only one character.
bsOnly :: Word8 -> BS
bsOnly = S.singleton
{-# INLINE bsOnly #-}

-- | Gets the length of a byte string.
bsLen :: BS -> Int
bsLen = S.length
{-# INLINE bsLen #-}

-- | Converts a list of characters to a byte string.
bsPack :: [Word8] -> BS
bsPack = S.pack
{-# INLINE bsPack #-}

-- | Converts a byte string to a list of characters.
bsUnpack :: BS -> [Word8]
bsUnpack = S.unpack
{-# INLINE bsUnpack #-}

-- | Concatenates byte strings into one.
bsConcat :: [BS] -> BS
bsConcat = S.concat
{-# INLINE bsConcat #-}

-- | Gets the character at an index in a byte string.
bsIndex :: BS -> Int -> Word8
bsIndex = S.index
{-# INLINE bsIndex #-}

-- | Gets the index of a character in a byte string.
bsElemIndex :: Word8 -> BS -> Maybe Int
bsElemIndex = S.elemIndex
{-# INLINE bsElemIndex #-}

-- | Converts a bytestring to lowercase.
bsLower :: BS -> BS
bsLower = S.map chrToLower

-- | Determines if a bytestring is a case-insensitive prefix of another.
bsPrefixCI :: BS -> BS -> Bool
bsPrefixCI x y = bsLower x `S.isPrefixOf` bsLower y

-- | Selects a substring for a byte string.
bsPart :: Int -> Int -> BS -> BS
bsPart offset len = S.take len . S.drop offset

-- | Returns the last word of a bytestring.
bsLast :: BS -> Maybe Word8
bsLast x
  | bsLen x > 0 = Just $ S.last x
  | otherwise = Nothing

-- | Takes characters from a byte string.
bsTake :: Int -> BS -> BS
bsTake = S.take
{-# INLINE bsTake #-}

-- | Drops characters from a byte string.
bsDrop :: Int -> BS -> BS
bsDrop = S.drop
{-# INLINE bsDrop #-}

-- | Removes a character from the end of a byte string.
bsUncons :: BS -> Maybe (Word8, BS)
bsUncons = S.uncons
{-# INLINE bsUncons #-}

-- | Converts a string to a byte string.
bcPack :: String -> BS
bcPack = C.pack
{-# INLINE bcPack #-}

-- | Converts a bytestring to a string.
bcUnpack :: BS -> String
bcUnpack = C.unpack
{-# INLINE bcUnpack #-}
