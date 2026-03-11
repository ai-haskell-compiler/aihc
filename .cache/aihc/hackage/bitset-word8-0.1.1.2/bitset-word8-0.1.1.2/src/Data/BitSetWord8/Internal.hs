{-|
Module      : Network.BitSetWord8.Internal
Copyright   : (c) Naoto Shimazaki 2017
License     : MIT (see the file LICENSE)

Maintainer  : https://github.com/nshimaza
Stability   : experimental

Space efficient set of 'Word8' and some pre-canned sets useful for parsing HTTP related 'ByteString'.
This file contains additional useful character sets but they aren't evaluated at compile time.
-}

{-# LANGUAGE DeriveLift #-}

module Data.BitSetWord8.Internal where

import           Data.Bits                  (setBit, shiftR, testBit)
import           Data.Char                  (chr, ord)
import           Data.List                  (foldl', splitAt)
import           Data.Semigroup             ((<>))
import qualified Data.Set                   as Set (Set, fromList, member)
import           Data.Word                  (Word64, Word8)
import           Language.Haskell.TH.Syntax (Lift)


-- | Bitwise set of Word8.  Space efficient backend and O(1) membership test.
data BitSetWord8 = BitSetWord8 {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64
                               {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64
    deriving (Eq, Lift, Show)

-- | DIGIT of RFC5324 in 'Char' list.
rfc5234Digit' :: [Char]
rfc5234Digit' = ['0'..'9']

-- | UPALPHA of RFC2616 in 'Char' list.
--   Note that RFC2616 has been obsoleted by RFC7230 and RFC7230 doesn't define UPALPHA.
rfc2616UpAlpha' :: [Char]
rfc2616UpAlpha' = [ 'A'..'Z' ]

-- | LOALPHA of RFC2616 in 'Char' list.
--   Note that RFC2616 has been obsoleted by RFC7230 and RFC7230 doesn't define LOALPHA.
rfc2616LoAlpha' :: [Char]
rfc2616LoAlpha' = [ 'a'..'z' ]

-- | ALPHA of RFC5324 in 'Char' list.
rfc5234Alpha' :: [Char]
rfc5234Alpha' = rfc2616UpAlpha' <> rfc2616LoAlpha'

-- | HEXDIG of RFC5324 in 'Char' list.
rfc5234HexDig' :: [Char]
rfc5234HexDig' = rfc5234Digit' <> ['A'..'F']

-- | VCHAR of RFC5324 in 'Char' list.
rfc5234VChar' :: [Char]
rfc5234VChar' = [ '!'..'~']

-- | WSP (aka white space) of RFC5324 in 'Char' list.
rfc5324Wsp' :: [Char]
rfc5324Wsp' = [ '\t', ' ' ]

-- | sub-delim of RFC3986 in 'Char' list.
rfc3986SubDelims' :: [Char]
rfc3986SubDelims' = [ '!', '$', '&', '\'', '(', ')', '*', '+', ',', ';', '=' ]

-- | gen-delim of RFC3986 in 'Char' list.
rfc3986GenDelims' :: [Char]
rfc3986GenDelims' = [ ':', '/', '?', '#', '[', ']', '@']

-- | reserved of RFC3986 in 'Char' list.
rfc3986Reserved' :: [Char]
rfc3986Reserved' = rfc3986GenDelims' <> rfc3986SubDelims'

-- | unreserved of RFC3986 in 'Char' list.
rfc3986Unreserved' :: [Char]
rfc3986Unreserved' = rfc5234Alpha' <> rfc5234Digit' <> [ '-', '.', '_', '~' ]

-- | pct-encoded of RFC3986 in 'Char' list.
rfc3986PctEncodedChar' :: [Char]
rfc3986PctEncodedChar' = ['%'] <> rfc5234HexDig'

-- | pchar of RFC3986 in 'Char' list.
rfc3986PChar' :: [Char]
rfc3986PChar' = rfc3986Unreserved' <> rfc3986PctEncodedChar' <> rfc3986SubDelims' <> [':', '@']

-- | URI-Reference of RFC3986 in 'Char' list.
rfc3986UriReference' :: [Char]
rfc3986UriReference' = rfc3986Reserved' <> rfc3986Unreserved' <> ['%']

-- | tchar of RFC7230 in 'Char' list.
rfc7230TChar' :: [Char]
rfc7230TChar' = [ '!', '#', '$', '%', '&', '\'', '*', '+', '-', '.', '^', '_', '`', '|', '~' ]
    <> rfc5234Digit' <> rfc5234Alpha'

-- | obs-text of RFC7230 in 'Char' list.
rfc7230ObsText' :: [Char]
rfc7230ObsText' = [ chr 0x80 .. chr 0xff]

-- | qdtext of RFC7230 in 'Char' list.
rfc7230QDText' :: [Char]
rfc7230QDText' = rfc5324Wsp' <> [ '!' ] <> [ '#' .. '[' ] <> [ ']' .. '~'] <> rfc7230ObsText'

-- | quoted-pair of RFC7230 in 'Char' list.
rfc7230QuotedPair' :: [Char]
rfc7230QuotedPair' = rfc5324Wsp' <> rfc5234VChar' <> rfc7230ObsText'

-- | O(1).  Return 'True' if given 'Word8' is a member of given 'BitSetWord8'.
member :: BitSetWord8 -> Word8 -> Bool
member bitSet val = doMember bitSet (val `div` 64) (val `mod` 64)
  where
    doMember :: BitSetWord8 -> Word8 -> Word8 -> Bool
    doMember (BitSetWord8 w _ _ _) 0 ind = testBit w (fromIntegral ind)
    doMember (BitSetWord8 _ w _ _) 1 ind = testBit w (fromIntegral ind)
    doMember (BitSetWord8 _ _ w _) 2 ind = testBit w (fromIntegral ind)
    doMember (BitSetWord8 _ _ _ w) 3 ind = testBit w (fromIntegral ind)
    doMember _                     _ _   = error "Impossible happen.  Word8 `dev` 64 cannot be greater than 3."

-- | Convert given list of 'Char' into 'Set' of 'Word8'.  Any 'Char' having code point greater than 0xff is ignored.
toWord8Set :: [Char] -> Set.Set Word8
toWord8Set = Set.fromList . map fromIntegral . filter (<= fromIntegral (maxBound :: Word8)) . map ord

-- | Convert 'Set' of Word8 to full filled list of boolean existence flag.
toBoolList :: Set.Set Word8 -> [Bool]
toBoolList wSet = map (\w -> Set.member w wSet) [0..0xff]

-- | Pack 64 of boolean list into single 'Word64' bitwise set.
toWord64 :: [Bool] -> Word64
toWord64 = foldl' (\a e -> let aL = shiftR a 1 in if e == True then setBit aL 63 else aL) 0

-- | Convert full filled boolean list into 4 packed 'Word64' list.
toWord64List :: [Bool] -> [Word64]
toWord64List [] = []
toWord64List bs = let (bs64, rest) = splitAt 64 bs in toWord64 bs64 : toWord64List rest

{- |
    Convert given List of 'Char' into packed bitwise set of Word64.
    Any 'Char' having code point greater than 0xff is ignored.
-}
fromList :: [Char] -> BitSetWord8
fromList = fromWord64List . toWord64List . toBoolList . toWord8Set
  where
    -- Convert 4 packed 'Word64' list into single 'BitSetWord8'
    fromWord64List :: [Word64] -> BitSetWord8
    fromWord64List (w0:w1:w2:w3:_) = BitSetWord8 w0 w1 w2 w3
    fromWord64List [_, _, _]       = raiseError
    fromWord64List [_, _]          = raiseError
    fromWord64List [_]             = raiseError
    fromWord64List []              = raiseError

    raiseError      = error "Impossible happen.  Arg of fromList must be converted to 4 elements list."
