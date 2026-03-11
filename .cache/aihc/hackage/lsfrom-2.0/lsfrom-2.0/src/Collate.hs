{-# LANGUAGE ForeignFunctionInterface #-}

-- with help from Gemini 2.0
-- first CString version: https://g.co/gemini/share/5eedf4ddf81e

module Collate (
  before,
  beforeUnsafe,
  ccollate)
where

import Foreign.C.String
import Foreign.C.Types
import System.IO.Unsafe (unsafePerformIO)

-- | Compares two wide strings using the current locale.
--
-- Returns:
--   - A negative integer if the first string is less than the second.
--   - Zero if the two strings are equal.
--   - A positive integer if the first string is greater than the second.
foreign import ccall safe "wcscoll"
  wcscoll :: CWString -> CWString -> CInt

-- | A safe wrapper around 'wcscoll' that takes Haskell String's and
-- handles the conversion to CWString's.
-- Requires that locale (LC_COLLATE) is set correctly.
wcscoll' :: String -> String -> IO CInt
wcscoll' s1 s2 = do
  withCWString s1 $ \cs1 ->
    withCWString s2 $ \cs2 ->
      return $ wcscoll cs1 cs2

-- |  Higher level function that returns an 'Ordering'.
ccollate :: String -> String -> IO Ordering
ccollate s1 s2 = do
  result <- wcscoll' s1 s2
  return $ compare result 0

-- | "ABC" `before` "DEF"
before :: String -> String -> IO Bool
before s1 s2 = do
  res <- ccollate s1 s2
  return $ res == LT

-- | unsafe wrapper
beforeUnsafe :: String -> String -> Bool
beforeUnsafe s1 s2 = unsafePerformIO $ before s1 s2
