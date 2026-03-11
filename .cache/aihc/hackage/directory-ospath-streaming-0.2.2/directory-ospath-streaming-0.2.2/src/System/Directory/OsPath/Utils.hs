-- |
-- Module:     System.Directory.OsPath.Utils
-- Copyright:  (c) Sergey Vinokurov 2024
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE UnboxedTuples #-}

module System.Directory.OsPath.Utils
  ( touch
  ) where

import GHC.Exts (touch#)
import GHC.IO (IO(..))

touch :: x -> IO ()
touch x = IO $ \s0 -> case touch# x s0 of s1 -> (# s1, () #)
