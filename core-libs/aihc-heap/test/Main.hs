-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE MagicHash #-}

module Main (main) where

import Control.Exception (SomeException, evaluate, try)
import GHC.Exts (Int (I#))
import GHC.Exts.Heap
import GHC.Exts.Heap.Constants (wORD_SIZE)
import GHC.Exts.Stack (decodeStack)
import GHC.Stack.CloneStack (StackSnapshot)

-- | Run an action and report whether it raised an exception.
raises :: IO a -> IO Bool
raises action = do
  result <- try (action >>= evaluate)
  pure $ case result of
    Left (_ :: SomeException) -> True
    Right _ -> False

expect :: String -> Bool -> IO ()
expect label ok = if ok then pure () else error (label ++ " failed")

-- | AIHC has no value of this type. The decoder must raise an exception
-- before it reads the argument.
noSnapshot :: StackSnapshot
noSnapshot = error "no stack snapshot"

main :: IO ()
main = do
  expect "wORD_SIZE" (wORD_SIZE == 8)
  expect "decodeStack raises" =<< raises (decodeStack noSnapshot)
  expect "peekItbl raises" =<< raises (peekItbl undefined)
  expect "getClosureData raises" =<< raises (getClosureData (Just 'x'))
  case 5 of
    I# n -> do
      closure <- getClosureData n
      case closure of
        IntClosure {intVal = 5} -> pure ()
        _ -> error "unboxed Int decoded incorrectly"
