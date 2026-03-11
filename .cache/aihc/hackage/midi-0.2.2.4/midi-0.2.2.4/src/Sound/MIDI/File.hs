{- |
MIDI-File Datatype

Taken from Haskore.
-}

module Sound.MIDI.File (
   T(..), Division(..), Track, Type(..),
   empty,
   ElapsedTime, fromElapsedTime, toElapsedTime,
   Tempo,       fromTempo,       toTempo,
   explicitNoteOff, implicitNoteOff,
   getTracks, mergeTracks, mapTrack,
   secondsFromTicks, ticksPerQuarterNote,

   showLines, changeVelocity, resampleTime,
   showEvent, showTime,
   sortEvents, progChangeBeforeSetTempo,
   ) where

import qualified Sound.MIDI.Message.Channel.Voice as VoiceMsg
import qualified Sound.MIDI.Message.Channel as ChannelMsg
import qualified Sound.MIDI.File.Event.Meta as MetaEvent
import qualified Sound.MIDI.File.Event as Event
import Sound.MIDI.File.Event.Meta (
   ElapsedTime, fromElapsedTime, toElapsedTime,
   Tempo,       fromTempo,       toTempo,
   )


import qualified Data.EventList.Relative.TimeBody as EventList
import qualified Numeric.NonNegative.Wrapper as NonNegW
import qualified Numeric.NonNegative.Class as NonNeg

import Test.QuickCheck (Arbitrary(arbitrary, shrink), )
import qualified Test.QuickCheck as QC

import qualified Control.Monad.Trans.State as MS
import Control.Monad (liftM, liftM2, )
import Sound.MIDI.String (rightS, )

import Data.Ratio((%))
import Data.Ix(Ix)
import Data.List(groupBy, sort)
import Data.Maybe(fromMaybe)

{- |
The datatypes for MIDI Files and MIDI Events
-}

data T = Cons Type Division [Track] deriving (Show, Eq)

data Type     = Mixed | Parallel | Serial
     deriving (Show, Eq, Ord, Ix, Enum, Bounded)
data Division = Ticks Tempo | SMPTE Int Int
     deriving (Show, Eq)

type Track = EventList.T ElapsedTime Event.T


{- |
An empty MIDI file.
Tempo is set to one tick per quarter note.
-}
empty :: T
empty = Cons Mixed (Ticks 1) [EventList.empty]


instance Arbitrary T where
   arbitrary =
      do (typ, content) <-
             QC.oneof $
                fmap (\track -> (Mixed, [track])) arbitrary :
                fmap (\tracks -> (Parallel, tracks)) arbitrary :
                fmap (\tracks -> (Serial, tracks)) arbitrary :
                []
         division <- arbitrary
         return (Cons typ division content)
   shrink (Cons typ division tracks) =
      map (Cons typ division) $ shrink tracks

instance Arbitrary Division where
   arbitrary =
      QC.oneof $
         liftM  (Ticks . (1+) . flip mod 32767) arbitrary :
         liftM2 (\x y -> SMPTE (1 + mod x 127) (1 + mod y 255)) arbitrary arbitrary :
         []



{- * Processing -}

{- |
Apply a function to each track.
-}
mapTrack :: (Track -> Track) -> T -> T
mapTrack f (Cons mfType division tracks) =
   Cons mfType division (map f tracks)

{- |
Convert all @NoteOn p 0@ to @NoteOff p 64@.
The latter one is easier to process.
-}
explicitNoteOff :: T -> T
explicitNoteOff =
   mapTrack (EventList.mapBody (Event.mapVoice VoiceMsg.explicitNoteOff))


{- |
Convert all @NoteOff p 64@ to @NoteOn p 0@.
The latter one can be encoded more efficiently using the running status.
-}
implicitNoteOff :: T -> T
implicitNoteOff =
   mapTrack (EventList.mapBody (Event.mapVoice VoiceMsg.implicitNoteOff))


getTracks :: T -> [Track]
getTracks (Cons _ _ trks) = trks

{- |
Merge all tracks into a single track
according to the MIDI file type.
-}
mergeTracks ::
   (NonNeg.C time) =>
   Type ->
   [EventList.T time event] ->
   EventList.T time event
mergeTracks typ tracks =
   case typ of
      Mixed    -> foldr (EventList.mergeBy (\_ _ -> True)) EventList.empty tracks
      Parallel -> foldr (EventList.mergeBy (\_ _ -> True)) EventList.empty tracks
      Serial   -> EventList.concat tracks

{- |
Process and remove all @SetTempo@ events.
The result is an event list where the times are measured in seconds.
-}
secondsFromTicks ::
   Division ->
   EventList.T ElapsedTime Event.T ->
   EventList.T NonNegW.Rational Event.T
secondsFromTicks division =
   EventList.catMaybes .
   flip MS.evalState MetaEvent.defltTempo .
   EventList.mapM
      (\ticks -> do
         microsPerQN <- MS.get
         -- cf. Standard MIDI Files 1.0, page 14
         return $
            NonNegW.fromNumberMsg "MIDI.File.processTempo" $
            fromElapsedTime ticks * fromIntegral (NonNegW.toNumber microsPerQN)
               % (1000000 * fromIntegral (NonNegW.toNumber (ticksPerQuarterNote division))))
      (\ev ->
         case ev of
            Event.MetaEvent (MetaEvent.SetTempo microsPerQN) ->
               MS.put microsPerQN >> return Nothing
            _ -> return $ Just ev)


ticksPerQuarterNote :: Division -> Tempo
ticksPerQuarterNote division =
   case division of
      Ticks ticksPerQN -> ticksPerQN
      SMPTE framesPerSecond ticksPerFrames ->
         {-
         I am uncertain, whether this is correct.
         The "Standard MIDI File 1.0" is unprecise
         with respect to the question,
         whether SetTempo is relevant also in SMPTE mode.
         TiMidity-2.13.2 interprets this kind of division as we do
         and qualifies it as "totally untested".
         -}
         NonNegW.fromNumberMsg "MIDI.File.ticksPerQuarterNote" $
         framesPerSecond * ticksPerFrames



{- * Debugging -}

{-# DEPRECATED
      showLines, changeVelocity, resampleTime,
      showEvent, showTime,
      sortEvents, progChangeBeforeSetTempo
         "only use this for debugging" #-}

{- |
Show the 'T' with one event per line,
suited for comparing MIDIFiles with @diff@.
Can this be replaced by 'Sound.MIDI.Load.showFile'?
-}
showLines :: T -> String
showLines (Cons mfType division tracks) =
   let showTrack track =
          unlines
             ("    (" :
              map
                 (\event -> "      " ++ show event ++ " :")
                 (EventList.toPairList track) ++
              "    []) :" :
              [])
   in  "MIDIFile.Cons " ++ show mfType ++ " (" ++ show division ++ ") (\n" ++
       concatMap showTrack tracks ++
       "  [])"

showTime :: ElapsedTime -> ShowS
showTime t =
   rightS 10 (shows t) . showString " : "

showEvent :: Event.T -> ShowS
showEvent (Event.MIDIEvent e) =
   showString "Event.MIDIEvent " . shows e
showEvent (Event.MetaEvent e) =
   showString "Event.MetaEvent " . shows e
showEvent (Event.SystemExclusive s) =
   showString "SystemExclusive " . shows s


{- |
A hack that changes the velocities by a rational factor.
-}

changeVelocity :: Double -> T -> T
changeVelocity r =
   let multVel vel =
          VoiceMsg.toVelocity $
          round (r * fromIntegral (VoiceMsg.fromVelocity vel))
       procVoice (VoiceMsg.NoteOn  pitch vel) = VoiceMsg.NoteOn  pitch (multVel vel)
       procVoice (VoiceMsg.NoteOff pitch vel) = VoiceMsg.NoteOff pitch (multVel vel)
       procVoice me = me
   in  mapTrack (EventList.mapBody (Event.mapVoice procVoice))

{- |
Change the time base.
-}

resampleTime :: Double -> T -> T
resampleTime r =
   let divTime  time = round (fromIntegral time / r)
       newTempo tmp  = round (fromIntegral tmp  * r)
       procEvent ev =
          case ev of
             Event.MetaEvent (MetaEvent.SetTempo tmp) ->
                Event.MetaEvent (MetaEvent.SetTempo (newTempo tmp))
             _ -> ev
   in  mapTrack (EventList.mapBody procEvent . EventList.mapTime divTime)

{- |
Sort MIDI note events lexicographically.
This is to make MIDI files unique
and robust against changes in the computation.
In principle Performance.merge should handle this
but due to rounding errors in Float
the order of note events still depends on some internal issues.
The sample rate of MIDI events should be coarse enough
to assert unique results.
-}

sortEvents :: T -> T
sortEvents =
   let coincideNote ev0 ev1 =
          fromMaybe False $
             do (_,x0) <- Event.maybeVoice ev0
                (_,x1) <- Event.maybeVoice ev1
                return (VoiceMsg.isNote x0 && VoiceMsg.isNote x1)
{-
       coincideNote
          (Event.MIDIEvent (ChannelMsg.Cons _ (ChannelMsg.Voice x0)))
          (Event.MIDIEvent (ChannelMsg.Cons _ (ChannelMsg.Voice x1))) =
          VoiceMsg.isNote x0 && VoiceMsg.isNote x1
       coincideNote _ _ = False
-}
       sortTrack =
          EventList.flatten . EventList.mapBody sort .
          EventList.mapCoincident (groupBy coincideNote)
   in  mapTrack sortTrack

{- |
Old versions of "Haskore.Interface.MIDI.Write"
wrote 'MIDIEvent.ProgramChange' and 'MetaEvent.SetTempo'
once at the beginning of a file in that order.
The current version supports multiple 'MIDIEvent.ProgramChange's in a track and
thus a 'MIDIEvent.ProgramChange' is set immediately before a note.
Because of this a 'MIDIEvent.ProgramChange' is now always after a 'MetaEvent.SetTempo'.
For checking equivalence with old MIDI files we can switch this back.
-}

progChangeBeforeSetTempo :: T -> T
progChangeBeforeSetTempo =
   let sortTrack evs =
          do ((t0,st@(Event.MetaEvent (MetaEvent.SetTempo _))), rest0)
                   <- EventList.viewL evs
             ((t1,pc@(Event.MIDIEvent (ChannelMsg.Cons _
                (ChannelMsg.Voice (VoiceMsg.ProgramChange _))))), rest1)
                   <- EventList.viewL rest0
             return $
                EventList.cons t0 pc $
                EventList.cons 0  st $
                EventList.delay t1 rest1
   in  mapTrack (\track -> fromMaybe track (sortTrack track))
