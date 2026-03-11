{- |
Channel messages
-}
module Sound.MIDI.Message.Channel (
   T(..), Body(..), get, getWithStatus, put, putWithStatus,
   Channel, fromChannel,   toChannel,

   Voice.Pitch,      Voice.fromPitch,      Voice.toPitch,
   Voice.Velocity,   Voice.fromVelocity,   Voice.toVelocity,
   Voice.Program,    Voice.fromProgram,    Voice.toProgram,
   Voice.Controller, Voice.fromController, Voice.toController,

   decodeStatus,
   ) where

import qualified Sound.MIDI.Message.Channel.Voice as Voice
import qualified Sound.MIDI.Message.Channel.Mode  as Mode

import qualified Sound.MIDI.Parser.Status as StatusParser
import           Sound.MIDI.Parser.Primitive
import qualified Sound.MIDI.Parser.Class as Parser
import Sound.MIDI.Parser.Status (Channel, fromChannel, toChannel, )

import Control.Monad (liftM, liftM2, when, )

import qualified Sound.MIDI.Writer.Status as StatusWriter
import qualified Sound.MIDI.Writer.Basic  as Writer
import qualified Sound.MIDI.Bit as Bit

import Sound.MIDI.Monoid ((+#+))

import Data.Tuple.HT (mapSnd, )

import Test.QuickCheck (Arbitrary(arbitrary, shrink), )
import qualified Test.QuickCheck as QC



data T = Cons {
     messageChannel :: Channel,
     messageBody    :: Body
   }
     -- ToDo: make nicer Show instance
     deriving (Show, Eq, Ord)

data Body =
     Voice Voice.T
   | Mode  Mode.T
     deriving (Show, Eq, Ord)

instance Arbitrary T where
   arbitrary =
      liftM2 Cons
         (liftM toChannel $ QC.frequency $
            -- we have to prefer one favorite channel in order to test correct implementation of the running status
            (20, return 3) :
            ( 1, QC.choose (0,15)) :
            [])
         (QC.frequency $
            (20, liftM Voice arbitrary) :
            ( 1, liftM Mode  arbitrary) :
            [])
   shrink (Cons chan body) =
      map (uncurry Cons) $
      case body of
         Voice v -> map (mapSnd Voice) $ shrink (chan, v)
         Mode  m -> map (mapSnd Mode)  $ shrink (chan, m)


-- * serialization

{- |
Parse an event.
Note that in the case of a regular MIDI Event, the tag is the status,
and we read the first byte of data before we call 'get'.
In the case of a MIDIEvent with running status,
we find out the status from the parser
(it's been nice enough to keep track of it for us),
and the tag that we've already gotten is the first byte of data.
-}
getWithStatus :: Parser.C parser => Int -> Parser.Fragile (StatusParser.T parser) T
getWithStatus tag =
   do (status@(code, channel), firstData) <-
         if tag < 0x80
           then maybe
                   (Parser.giveUp "messages wants to repeat status byte, but there was no status yet")
                   (\cc -> return (cc,tag))
                      =<< StatusParser.get
           else liftM ((,) $ decodeStatus tag) $ StatusParser.lift get1
      StatusParser.set (Just status)
      StatusParser.lift $ get code channel firstData

-- | for internal use
decodeStatus :: Int -> (Int, Channel)
decodeStatus  =  mapSnd toChannel . Bit.splitAt 4

{- |
Parse a MIDI Channel message.
Note that since getting the first byte is a little complex
(there are issues with running status),
the code, channel and first data byte
must be determined by the caller.
-}
get :: Parser.C parser => Int -> Channel -> Int -> Parser.Fragile parser T
get code channel firstData =
   liftM (Cons channel) $
   if code == 11 && firstData >= 0x78
     then
        when (firstData >= 0x80)
           (Parser.giveUp ("mode value out of range: " ++ show firstData)) >>
        liftM Mode (Mode.get firstData)
     else liftM Voice (Voice.get code firstData)


put :: Writer.C writer => T -> writer
put = StatusWriter.toWriterWithoutStatus . putWithStatus

putWithStatus ::
   (StatusWriter.Compression compress, Writer.C writer) =>
   T -> StatusWriter.T compress writer
putWithStatus (Cons c e) =
   case e of
      Voice v -> Voice.putWithStatus (putChannel c) v
      Mode  m -> putChannel c 11 +#+ StatusWriter.lift (Mode.put m)

-- | output a channel + message code
putChannel ::
   (StatusWriter.Compression compress, Writer.C writer) =>
   Channel -> Int -> StatusWriter.T compress writer
putChannel chan code =
   StatusWriter.change (code, chan) $
      Writer.putIntAsByte (16*code + fromChannel chan)
