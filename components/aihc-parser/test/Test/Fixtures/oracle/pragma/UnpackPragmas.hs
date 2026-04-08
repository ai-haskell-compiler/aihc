{- ORACLE_TEST pass -}
module UnpackPragmas where

import Data.Word (Word8, Word32)

data Pair = Pair {-# UNPACK #-} !Int {-# UNPACK #-} !Int

data Nested = Nested {-# UNPACK #-} !Inner

data Inner = Inner {-# UNPACK #-} !Int {-# UNPACK #-} !Int

data WordPacked = WordPacked
  {-# UNPACK #-} !Word32
  {-# UNPACK #-} !Word8
  {-# UNPACK #-} !Word32
