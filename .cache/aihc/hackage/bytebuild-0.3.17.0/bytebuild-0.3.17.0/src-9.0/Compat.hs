{-# LANGUAGE MagicHash #-}

-- This is actually used with both GHC 8.10 and with GHC 9.0.
-- The name of the directory is a little misleading.
module Compat
  ( int8ToInt#
  , int16ToInt#
  , int32ToInt#
  , wordToWord8#
  , wordToWord16#
  , wordToWord32#
  , word8ToWord#
  , word16ToWord#
  , word32ToWord#
  ) where

import GHC.Exts (Int#, Word#)

int8ToInt# :: Int# -> Int#
{-# INLINE int8ToInt# #-}
int8ToInt# x = x

int16ToInt# :: Int# -> Int#
{-# INLINE int16ToInt# #-}
int16ToInt# x = x

int32ToInt# :: Int# -> Int#
{-# INLINE int32ToInt# #-}
int32ToInt# x = x

wordToWord8# :: Word# -> Word#
{-# INLINE wordToWord8# #-}
wordToWord8# x = x

wordToWord16# :: Word# -> Word#
{-# INLINE wordToWord16# #-}
wordToWord16# x = x

wordToWord32# :: Word# -> Word#
{-# INLINE wordToWord32# #-}
wordToWord32# x = x

word8ToWord# :: Word# -> Word#
{-# INLINE word8ToWord# #-}
word8ToWord# x = x

word16ToWord# :: Word# -> Word#
{-# INLINE word16ToWord# #-}
word16ToWord# x = x

word32ToWord# :: Word# -> Word#
{-# INLINE word32ToWord# #-}
word32ToWord# x = x
