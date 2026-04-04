{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : Aihc.Cpp.Cursor
-- Description : Zero-copy cursor over ByteString for efficient scanning
-- License     : Unlicense
--
-- A lightweight cursor abstraction over a strict 'ByteString'. The cursor
-- tracks a position into a shared buffer, enabling O(1) peeking and
-- zero-copy slicing. All CPP-significant bytes are ASCII (0x00-0x7F),
-- so byte-level operations are safe; non-ASCII bytes (>= 0x80) can be
-- bulk-copied without decoding.
module Aihc.Cpp.Cursor
  ( Cursor (..),
    fromByteString,
    fromText,
    toText,
    null,
    peekByte,
    peekByte2,
    advance,
    advance2,
    sliceText,
    sliceSince,
    skipWhile,
    bufLength,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Word (Word8)
import Prelude hiding (null)

-- | A position-based cursor into a 'ByteString' buffer.
-- The buffer is shared across all cursors derived from the same input,
-- enabling zero-copy slicing.
data Cursor = Cursor
  { curBuf :: {-# UNPACK #-} !ByteString,
    curPos :: {-# UNPACK #-} !Int
  }
  deriving (Show)

-- | Create a cursor at the start of a 'ByteString'.
fromByteString :: ByteString -> Cursor
fromByteString bs = Cursor bs 0

-- | Create a cursor from 'Text' by encoding to UTF-8.
fromText :: Text -> Cursor
fromText = fromByteString . TE.encodeUtf8

-- | Decode the remaining bytes from the cursor position as UTF-8 'Text'.
toText :: Cursor -> Text
toText (Cursor buf pos) = TE.decodeUtf8 (BS.drop pos buf)

-- | Is the cursor at the end of input?
null :: Cursor -> Bool
null (Cursor buf pos) = pos >= BS.length buf
{-# INLINE null #-}

-- | Peek at the current byte without advancing. Returns 'Nothing' at end
-- of input.
peekByte :: Cursor -> Maybe Word8
peekByte (Cursor buf pos)
  | pos >= BS.length buf = Nothing
  | otherwise = Just (BS.index buf pos)
{-# INLINE peekByte #-}

-- | Peek at the current and next byte without advancing. Returns 'Nothing'
-- if fewer than 2 bytes remain.
peekByte2 :: Cursor -> Maybe (Word8, Word8)
peekByte2 (Cursor buf pos)
  | pos + 1 >= BS.length buf = Nothing
  | otherwise = Just (BS.index buf pos, BS.index buf (pos + 1))
{-# INLINE peekByte2 #-}

-- | Advance the cursor by one byte.
advance :: Cursor -> Cursor
advance (Cursor buf pos) = Cursor buf (pos + 1)
{-# INLINE advance #-}

-- | Advance the cursor by two bytes.
advance2 :: Cursor -> Cursor
advance2 (Cursor buf pos) = Cursor buf (pos + 2)
{-# INLINE advance2 #-}

-- | Extract a zero-copy 'Text' slice from position @start@ to position
-- @end@ (exclusive) in the cursor's buffer.
sliceText :: Int -> Int -> Cursor -> Text
sliceText start end (Cursor buf _) =
  TE.decodeUtf8 (BS.take (end - start) (BS.drop start buf))
{-# INLINE sliceText #-}

-- | Extract a zero-copy 'Text' slice from the given start position to
-- the cursor's current position.
sliceSince :: Int -> Cursor -> Text
sliceSince start cur = sliceText start (curPos cur) cur
{-# INLINE sliceSince #-}

-- | Advance the cursor while the predicate holds for the current byte.
skipWhile :: (Word8 -> Bool) -> Cursor -> Cursor
skipWhile p = go
  where
    go !cur = case peekByte cur of
      Just b | p b -> go (advance cur)
      _ -> cur
{-# INLINE skipWhile #-}

-- | Total length of the underlying buffer.
bufLength :: Cursor -> Int
bufLength (Cursor buf _) = BS.length buf
{-# INLINE bufLength #-}
