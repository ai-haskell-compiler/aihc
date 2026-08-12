{-# LANGUAGE MagicHash #-}

module GHC.Word
  ( Word (..),
    Word8 (..),
    Word16 (..),
    Word32 (..),
    Word64 (..),
  )
where

data Word = W# Word#

data Word8 = W8# Word8#

data Word16 = W16# Word16#

data Word32 = W32# Word32#

data Word64 = W64# Word64#
