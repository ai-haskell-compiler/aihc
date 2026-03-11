{- |
System Real Time messages
-}
module Sound.MIDI.Message.System.RealTime (
   T(..), get, put,
   ) where

import qualified Sound.MIDI.Parser.Class as Parser

import qualified Sound.MIDI.Writer.Basic as Writer

import Data.Ix(Ix)


data T =
     TimingClock                   -- F8
   | Start                         -- FA
   | Continue                      -- FB
   | Stop                          -- FC
   | ActiveSensing                 -- FE
   | Reset                         -- FF
   deriving (Eq, Ord, Show, Enum, Ix)


-- * serialization

get :: Parser.C parser => Int -> Parser.Fragile parser T
get code =
   case code of
      0xF8 -> return TimingClock
      0xFA -> return Start
      0xFB -> return Continue
      0xFC -> return Stop
      0xFE -> return ActiveSensing
      0xFF -> return Reset
      _    -> Parser.giveUp ("unknown System Real Time message code " ++ show code)

put :: Writer.C writer => T -> writer
put msg =
   case msg of
      TimingClock   -> Writer.putByte 0xF8
      Start         -> Writer.putByte 0xFA
      Continue      -> Writer.putByte 0xFB
      Stop          -> Writer.putByte 0xFC
      ActiveSensing -> Writer.putByte 0xFE
      Reset         -> Writer.putByte 0xFF
