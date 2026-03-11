module HaskellWorks.Data.Char.IsChar
    ( IsChar(..)
    ) where

import Data.Word

import qualified Data.ByteString.Internal as BI

class IsChar c where
  toChar :: c -> Char

instance IsChar Word8 where
  toChar = BI.w2c
  {-# INLINE toChar #-}

instance IsChar Char where
  toChar = id
  {-# INLINE toChar #-}
