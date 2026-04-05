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
    skipToInteresting,
    bufLength,

    -- * Line-oriented operations
    findNewline,
    skipNewline,
    lineSlice,
    startsWithByte,
    peekByteAt,
    atEnd,
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

-- | Check whether the cursor is at the end of input (synonym for 'null').
atEnd :: Cursor -> Bool
atEnd = null
{-# INLINE atEnd #-}

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

-- | Peek at a byte at an absolute position in the buffer.
peekByteAt :: Int -> Cursor -> Maybe Word8
peekByteAt absPos (Cursor buf _)
  | absPos < 0 || absPos >= BS.length buf = Nothing
  | otherwise = Just (BS.index buf absPos)
{-# INLINE peekByteAt #-}

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

-- | Advance the cursor past bytes that cannot start any CPP-significant
-- two-character sequence. Stops at: @\"@ (0x22), @\'@ (0x27), @*@ (0x2A),
-- @-@ (0x2D), @/@ (0x2F), @\\@ (0x5C), @{@ (0x7B), @}@ (0x7D),
-- or end of input. This allows bulk-copying runs of plain text
-- (identifiers, whitespace, operators, non-ASCII UTF-8) without
-- per-byte dispatch.
skipToInteresting :: Cursor -> Cursor
skipToInteresting = go
  where
    go !cur = case peekByte cur of
      Nothing -> cur
      Just b
        | isInteresting b -> cur
        | otherwise -> go (advance cur)

    isInteresting :: Word8 -> Bool
    isInteresting b =
      b == 0x22 -- '"'
        || b == 0x27 -- '\''
        || b == 0x2A -- '*'
        || b == 0x2D -- '-'
        || b == 0x2F -- '/'
        || b == 0x5C -- '\\'
        || b == 0x7B -- '{'
        || b == 0x7D -- '}'
    {-# INLINE isInteresting #-}
{-# INLINE skipToInteresting #-}

-- | Find the next newline byte (0x0A) or EOF. Returns a cursor
-- positioned at the newline (or at EOF). The bytes from the
-- original position to the returned position form the line content
-- (without the newline).
findNewline :: Cursor -> Cursor
findNewline = go
  where
    go !cur = case peekByte cur of
      Nothing -> cur
      Just 0x0A -> cur -- '\n'
      Just _ -> go (advance cur)
{-# INLINE findNewline #-}

-- | Advance past a newline byte if the cursor is currently on one.
-- Returns 'Nothing' at EOF, 'Just cursor' after the newline otherwise.
skipNewline :: Cursor -> Maybe Cursor
skipNewline cur = case peekByte cur of
  Just 0x0A -> Just (advance cur) -- '\n'
  _ -> Nothing
{-# INLINE skipNewline #-}

-- | Create a cursor that views only the bytes from @curPos cur@ to @end@
-- (exclusive) in the same buffer. This is a logical "line slice" — a
-- sub-cursor bounded at @end@.
--
-- Implementation: creates a new cursor over a sub-ByteString. The
-- sub-ByteString shares the underlying memory (zero-copy via 'BS.take'
-- and 'BS.drop').
lineSlice :: Int -> Cursor -> Cursor
lineSlice end (Cursor buf start) =
  Cursor (BS.take (end - start) (BS.drop start buf)) 0
{-# INLINE lineSlice #-}

-- | Check if the byte at the cursor's current position matches the
-- given byte.
startsWithByte :: Word8 -> Cursor -> Bool
startsWithByte b cur = peekByte cur == Just b
{-# INLINE startsWithByte #-}
