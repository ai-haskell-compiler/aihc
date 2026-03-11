module Sound.MIDI.File.Event.SystemExclusive
   (T(..), get, put, ) where

import Sound.MIDI.IO (ByteList)

import           Sound.MIDI.Parser.Primitive
import qualified Sound.MIDI.Parser.Class as Parser

import qualified Sound.MIDI.Writer.Basic as Writer
import Sound.MIDI.Monoid ((+#+))

import Control.Monad (liftM, )



{-# DEPRECATED T "implement this data type properly" #-}
{- |
There are three forms of System Exclusive Messages in MIDI files:
monolithic, chopped into packets, escape form (with unrestricted binary data).

Currently we only support first and last type explicitly.
But we leave the trailing 0xF7 markers
which can be used to detect whether the messages are actually meant as packets.

Since I don't know where manufacturer information is in the packets form,
I omit manufacturer handling for now.
-}
data T =
     Regular ByteList   -- F0
   | Escape  ByteList   -- F7
     deriving (Show, Eq, Ord)


get :: Parser.C parser => Int -> Parser.Fragile parser T
get tag =
   case tag of
      0xF0 -> liftM Regular $ getBigN =<< getVar
      0xF7 -> liftM Escape  $ getBigN =<< getVar
      _ -> Parser.giveUp "SystemExclusive: unkown message type"

put :: Writer.C writer => T -> writer
put sysex =
   case sysex of
      Regular bytes -> Writer.putByte 0xF0 +#+ Writer.putLenByteList bytes
      Escape  bytes -> Writer.putByte 0xF7 +#+ Writer.putLenByteList bytes
