module GHC.IO.IOMode
  ( IOMode (..),
    ioModeNumber,
    isReadableMode,
    isWritableMode,
  )
where

import Prelude

data IOMode
  = ReadMode
  | WriteMode
  | AppendMode
  | ReadWriteMode

ioModeNumber :: IOMode -> Int
ioModeNumber mode =
  case mode of
    ReadMode -> 0
    WriteMode -> 1
    AppendMode -> 2
    ReadWriteMode -> 3

isReadableMode :: IOMode -> Bool
isReadableMode mode =
  case mode of
    ReadMode -> True
    ReadWriteMode -> True
    _ -> False

isWritableMode :: IOMode -> Bool
isWritableMode mode =
  case mode of
    WriteMode -> True
    AppendMode -> True
    ReadWriteMode -> True
    _ -> False
