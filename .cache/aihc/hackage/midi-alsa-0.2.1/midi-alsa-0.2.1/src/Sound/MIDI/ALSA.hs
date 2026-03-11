{-
ToDo:
ALSA events may contain values
for channel, pitch, velocity, controller, program that are out of bound.
In this case our conversions yield an error.
-}
module Sound.MIDI.ALSA where

import qualified Sound.MIDI.Message.Channel as ChannelMsg
import qualified Sound.MIDI.Message.Channel.Voice as VoiceMsg
import qualified Sound.MIDI.Message.Channel.Mode as Mode

import Sound.MIDI.Message.Channel (Channel, )
import Sound.MIDI.Message.Channel.Voice (Velocity, Pitch, Controller, Program, )

import qualified Sound.ALSA.Sequencer.Event as Event

import qualified Data.Accessor.Basic as Acc
import Data.Accessor.Basic ((^.), )
import Data.Tuple.HT (mapPair, )


-- * value conversions

toChannel :: Event.Channel -> Channel
toChannel  =  ChannelMsg.toChannel . fromIntegral . Event.unChannel

fromChannel :: Channel -> Event.Channel
fromChannel  =  Event.Channel . fromIntegral . ChannelMsg.fromChannel


toPitch :: Event.Pitch -> Pitch
toPitch  =  ChannelMsg.toPitch . fromIntegral . Event.unPitch

fromPitch :: Pitch -> Event.Pitch
fromPitch  =  Event.Pitch . fromIntegral . ChannelMsg.fromPitch


toVelocity :: Event.Velocity -> Velocity
toVelocity  =  ChannelMsg.toVelocity . fromIntegral . Event.unVelocity

fromVelocity :: Velocity -> Event.Velocity
fromVelocity  =  Event.Velocity . fromIntegral . ChannelMsg.fromVelocity


{- |
Return a 'NoteOff' if input is a 'NoteOn' with velocity zero.
This is a trick of the MIDI standard
in order to allow compression of a series of note events.
After normalization you can safely match on 'NoteOn' and 'NoteOff'.
-}
normalizeNote :: (Event.NoteEv, Velocity) -> (Event.NoteEv, Velocity)
normalizeNote nv@(notePart,velocity) =
   case notePart of
      Event.NoteOn ->
         if velocity == VoiceMsg.toVelocity 0
           then (Event.NoteOff, VoiceMsg.normalVelocity)
           else (Event.NoteOn, velocity)
      _ -> nv

normalNoteFromEvent :: Event.NoteEv -> Event.Note -> (Event.NoteEv, Velocity)
normalNoteFromEvent notePart note =
   normalizeNote (notePart, note ^. noteVelocity)


{- |
Controllers from @0x78@ to @0x7F@ are special,
you must assert that the controller number is in the range @0@ to @0x77@.
-}
toController :: Event.Parameter -> Controller
toController  =  ChannelMsg.toController . fromIntegral . Event.unParameter

fromController :: Controller -> Event.Parameter
fromController  =  Event.Parameter . fromIntegral . ChannelMsg.fromController


toProgram :: Event.Value -> Program
toProgram  =  ChannelMsg.toProgram . fromIntegral . Event.unValue

fromProgram :: Program -> Event.Value
fromProgram  =  Event.Value . fromIntegral . ChannelMsg.fromProgram


-- * construction of event data records

noteEvent ::
   Channel -> Pitch -> Velocity -> Velocity -> Int ->
   Event.Note
noteEvent chan pitch velOn velOff dur =
   Event.Note
      (fromChannel chan)
      (fromPitch pitch)
      (fromVelocity velOn)
      (fromVelocity velOff)
      (Event.Duration $ fromIntegral dur)

controllerEvent ::
   Channel -> Controller -> Int ->
   Event.Ctrl
controllerEvent chan ctrl value =
   Event.Ctrl
      (fromChannel chan)
      (fromController ctrl)
      (Event.Value $ fromIntegral value)

programChangeEvent ::
   Channel -> Program ->
   Event.Ctrl
programChangeEvent chan pgm =
   Event.Ctrl
      (fromChannel chan)
      (Event.Parameter 0)
      (fromProgram pgm)

modeEvent ::
   Channel -> Mode.T ->
   Event.Ctrl
modeEvent chan m =
   case Mode.toControllerValue m of
      (param,value) ->
         Event.Ctrl
            (fromChannel chan)
            (Event.Parameter $ fromIntegral param)
            (Event.Value value)


-- * accessors to event data fields

noteChannel :: Acc.T Event.Note Channel
noteChannel =
   Acc.fromSetGet
      (\c note -> note{Event.noteChannel = fromChannel c})
      (toChannel . Event.noteChannel)

notePitch :: Acc.T Event.Note Pitch
notePitch =
   Acc.fromSetGet
      (\p note -> note{Event.noteNote = fromPitch p})
      (toPitch . Event.noteNote)

{- |
This may not yield what you expect.
See 'normalizeNote'.
-}
noteVelocity :: Acc.T Event.Note Velocity
noteVelocity =
   Acc.fromSetGet
      (\v note -> note{Event.noteVelocity = fromVelocity v})
      (toVelocity . Event.noteVelocity)


ctrlChannel :: Acc.T Event.Ctrl Channel
ctrlChannel =
   Acc.fromSetGet
      (\c ctrl -> ctrl{Event.ctrlChannel = fromChannel c})
      (toChannel . Event.ctrlChannel)

{- |
This is undefined, if the controller is no regular controller
but a channel mode message.
Better use 'ctrlControllerMode'.
-}
ctrlController :: Acc.T Event.Ctrl Controller
ctrlController =
   Acc.fromSetGet
      (\c ctrl -> ctrl{Event.ctrlParam = fromController c})
      (toController . Event.ctrlParam)

data ControllerMode =
     Controller Controller Int
   | Mode Mode.T
   deriving (Show, Eq)

ctrlControllerMode :: Acc.T Event.Ctrl ControllerMode
ctrlControllerMode =
   Acc.fromSetGet
      (\cm ctrl ->
         let (p,v) =
                case cm of
                   Controller c x ->
                      (fromController c, fromIntegral x)
                   Mode m ->
                      mapPair (Event.Parameter, fromIntegral) $
                      Mode.toControllerValue m
         in  ctrl{Event.ctrlParam = p,
                  Event.ctrlValue = Event.Value v})
      (\ctrl ->
         let c = Event.ctrlParam ctrl
         in  if c < Event.Parameter 0x78
               then Controller (ctrl ^. ctrlController) (ctrl ^. ctrlValue)
               else Mode $ snd $ Mode.fromControllerValue
                       (Event.unParameter $ Event.ctrlParam ctrl,
                        fromIntegral $ Event.unValue $ Event.ctrlValue ctrl))

ctrlValue :: Acc.T Event.Ctrl Int
ctrlValue =
   Acc.fromSetGet
      (\x ctrl -> ctrl{Event.ctrlValue = Event.Value $ fromIntegral x})
      (fromIntegral . Event.unValue . Event.ctrlValue)

ctrlProgram :: Acc.T Event.Ctrl Program
ctrlProgram =
   Acc.fromSetGet
      (\p ctrl -> ctrl{Event.ctrlValue = fromProgram p})
      (toProgram . Event.ctrlValue)
