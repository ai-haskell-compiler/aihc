{- |
System Common messages
-}
module Sound.MIDI.Message.System.Common (
   T(..), TimeNibbleType(..), get, put,
   ) where

import           Sound.MIDI.Parser.Primitive
import qualified Sound.MIDI.Parser.Class as Parser

import Control.Monad.Trans.Class (lift, )
import Control.Monad (liftM, liftM2, )

import qualified Sound.MIDI.Writer.Basic as Writer
import Sound.MIDI.Monoid ((+#+))

import qualified Sound.MIDI.Bit as Bit

import Data.Ix(Ix)


data T =
     TimeCodeQuarterFrame TimeNibbleType Int
   | SongPositionPointer Int
   | SongSelect Int
   | TuneRequest
--   | EndOfSystemExclusive

data TimeNibbleType =
     FrameLS
   | FrameMS
   | SecondsLS
   | SecondsMS
   | MinutesLS
   | MinutesMS
   | HoursLS
   | HoursMS -- ^ also includes SMPTE type
   deriving (Eq, Ord, Show, Enum, Ix)


-- * serialization

get :: Parser.C parser => Int -> Parser.Fragile parser T
get code =
   case code of
      0xF1 ->
         do dat <- get1
            let (nib, value)  = Bit.splitAt 4 dat
            let (msb, nibble) = Bit.splitAt 3 nib
            lift $ Parser.warnIf (msb/=0)
               "TimeCodeQuarterFrame: most significant bit must 0"
            return $ TimeCodeQuarterFrame (toEnum nibble) value
      0xF2 -> liftM2 (\lsb msb -> SongPositionPointer (lsb + Bit.shiftL 7 msb)) get1 get1
      0xF3 -> liftM SongSelect get1
      0xF6 -> return TuneRequest
      _    -> Parser.giveUp ("invalid System Common code:" ++ show code)

put :: Writer.C writer => T -> writer
put msg =
   case msg of
      TimeCodeQuarterFrame nibble value ->
         Writer.putByte 0xF1 +#+
         Writer.putIntAsByte (Bit.shiftL 4 (fromEnum nibble) + value)
      SongPositionPointer pos ->
         Writer.putByte 0xF2 +#+
            let (msb,lsb) = Bit.splitAt 7 pos
            in  Writer.putIntAsByte lsb +#+
                Writer.putIntAsByte msb
      SongSelect song ->
         Writer.putByte 0xF3 +#+
         Writer.putIntAsByte song
      TuneRequest ->
         Writer.putByte 0xF6
