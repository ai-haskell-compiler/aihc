{-# LANGUAGE MagicHash #-}

module Op
  ( writeCharArray#
  , copyByteArray#
  , copyMutableByteArray#
  ) where

import GHC.Exts (copyByteArray#, copyMutableByteArray#, writeCharArray#)
