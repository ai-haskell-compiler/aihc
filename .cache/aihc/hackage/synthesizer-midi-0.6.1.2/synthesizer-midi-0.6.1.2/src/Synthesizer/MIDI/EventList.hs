{-# LANGUAGE NoImplicitPrelude #-}
module Synthesizer.MIDI.EventList where

import qualified Sound.MIDI.Message.Class.Check as Check

import qualified Sound.MIDI.Message.Channel as ChannelMsg
import qualified Sound.MIDI.Message.Channel.Mode as Mode

import qualified Data.EventList.Relative.TimeBody  as EventList
import qualified Data.EventList.Relative.MixedBody as EventListMB
import qualified Data.EventList.Relative.BodyBody  as EventListBB

import Control.Monad.Trans.State
          (State, state, evalState, gets, put, )
import Data.Traversable (traverse, )

import qualified Numeric.NonNegative.Class   as NonNeg
import qualified Numeric.NonNegative.Wrapper as NonNegW
import qualified Numeric.NonNegative.Chunky as NonNegChunky

import Data.Array (Array, listArray, (!), bounds, inRange, )

import qualified Data.List.HT as ListHT
import Data.Tuple.HT (mapPair, mapFst, mapSnd, )
import Data.Maybe.HT (toMaybe, )
import Data.Maybe (catMaybes, isNothing, )
import Control.Monad.HT ((<=<), )
import Control.Monad (guard, msum, )

import NumericPrelude.Numeric
import NumericPrelude.Base


type StrictTime = NonNegW.Integer

{-
Maybe we could use StorableVector.Pattern.LazySize
or we could use synthesizer-core/ChunkySize.
What package should we rely on?
Which one is more portable?

We do not use this type for timing in event lists anymore.
It worked in principle but left us with a couple of memory leaks,
that I could never identify and eliminate completely.
-}
type LazyTime = NonNegChunky.T NonNegW.Integer



-- * event filters

type Filter event = State (EventList.T StrictTime [event])



{- |
We turn the strict time values into lazy ones
according to the breaks by our beat.
However for the laziness breaks we ignore the events that are filtered out.
That is we loose laziness granularity
but hopefully gain efficiency by larger blocks.
-}
getSlice ::
   (event -> Maybe a) ->
   Filter event (EventList.T StrictTime [a])
getSlice f =
   state $
   EventList.unzip .
   fmap (ListHT.partitionMaybe f)


type Channel    = ChannelMsg.Channel
type Controller = ChannelMsg.Controller
type Pitch      = ChannelMsg.Pitch
type Velocity   = ChannelMsg.Velocity
type Program    = ChannelMsg.Program


getControllerEvents ::
   (Check.C event) =>
   Channel -> Controller ->
   Filter event (EventList.T StrictTime [Int])
getControllerEvents chan ctrl =
   getSlice (Check.controller chan ctrl)

{-
getControllerEvents ::
   (Check.C event) =>
   Channel -> Controller ->
   Filter event (EventList.T StrictTime (Maybe Int))
getControllerEvents chan ctrl =
   fmap (fmap (fmap snd . ListHT.viewR)) $
   getSlice (Check.controller chan ctrl)
-}

data NoteBoundary a =
     NoteBoundary Pitch Velocity a
   | AllNotesOff
   deriving (Eq, Show)

data Note = Note Program Pitch Velocity LazyTime
   deriving (Eq, Show)


case_ :: Maybe a -> (a -> b) -> Maybe b
case_ = flip fmap

{-
We could also provide a function which filters for specific programs/presets.
-}
getNoteEvents ::
   (Check.C event) =>
   Channel ->
   Filter event (EventList.T StrictTime [Either Program (NoteBoundary Bool)])
getNoteEvents chan =
   getSlice $ checkNoteEvent chan

checkNoteEvent ::
   (Check.C event) =>
   Channel -> event ->
   Maybe (Either Program (NoteBoundary Bool))
checkNoteEvent chan e = msum $
   case_ (Check.noteExplicitOff chan e) (\(velocity, pitch, press) ->
      Right $ NoteBoundary pitch velocity press) :
   case_ (Check.program chan e) Left :
   {-
   We do not handle AllSoundOff here,
   since this would also mean to clear reverb buffers
   and this cannot be handled here.
   -}
   (Check.mode chan e >>= \mode -> do
      guard (mode == Mode.AllNotesOff)
      return (Right AllNotesOff)) :
   []

embedPrograms ::
   Program ->
   EventList.T StrictTime [Either Program (NoteBoundary Bool)] ->
   EventList.T StrictTime [NoteBoundary (Maybe Program)]
embedPrograms initPgm =
   fmap catMaybes .
   flip evalState initPgm .
   traverse (traverse embedProgramState)

embedProgramState ::
   Either Program (NoteBoundary Bool) ->
   State Program (Maybe (NoteBoundary (Maybe Program)))
embedProgramState =
   -- evaluate program for every event in order to prevent a space leak
   (\n -> state (\s -> (seq s n, s)))
   <=<
   either
      (\pgm -> put pgm >> return Nothing)
      (\bnd ->
         gets (Just .
         case bnd of
            AllNotesOff -> const AllNotesOff
            NoteBoundary p v press ->
               NoteBoundary p v . toMaybe press))


matchNoteEvents ::
   EventList.T StrictTime [NoteBoundary (Maybe Program)] ->
   EventList.T StrictTime [Note]
matchNoteEvents =
   matchNoteEventsCore $ \bndOn -> case bndOn of
      AllNotesOff -> Nothing
      NoteBoundary pitchOn velOn pressOn ->
         flip fmap pressOn $ \pgm ->
            (\bndOff ->
               case bndOff of
                  AllNotesOff -> True
                  NoteBoundary pitchOff _velOff pressOff ->
                     pitchOn == pitchOff && isNothing pressOff,
             Note pgm pitchOn velOn)

matchNoteEventsCore ::
   (noteBnd ->
    Maybe (noteBnd -> Bool, LazyTime -> Note)) ->
   EventList.T StrictTime [noteBnd] ->
   EventList.T StrictTime [Note]
matchNoteEventsCore methods =
   let recourseEvents =
          EventListMB.switchBodyL $ \evs0 xs0 -> case evs0 of
             [] -> ([], xs0)
             ev:evs ->
                case methods ev of
                   Nothing ->
                      recourseEvents (EventListMB.consBody evs xs0)
                   Just (check, cons) ->
                      case durationRemove check (EventListMB.consBody evs xs0) of
                         (dur, xs1) ->
                            mapFst
                               (cons dur :)
                               (recourseEvents xs1)
       recourse =
          EventList.switchL EventList.empty $ \(t,evs0) xs0 ->
          let (evs1,xs1) = recourseEvents (EventListMB.consBody evs0 xs0)
          in  EventList.cons t evs1 $ recourse xs1
   in  recourse


{-
durationRemove Char.isUpper ("a" ./ 3 /. "bf" ./ 5 /. "aCcd" ./ empty :: Data.EventList.Relative.BodyBody.T StrictTime [Char])
-}
{- |
Search for specific event,
return its time stamp and remove it.
-}
durationRemove ::
   (NonNeg.C time) =>
   (body -> Bool) ->
   EventListBB.T time [body] ->
   (NonNegChunky.T time, EventListBB.T time [body])
durationRemove p =
   let errorEndOfList =
          (error "no matching body element found",
           error "list ended before matching element found")
       recourse =
          EventListMB.switchBodyL $ \evs xs0 ->
          let (prefix, suffix0) = break p evs
              (suffix1, rest) =
                 case suffix0 of
                    [] -> ([],
                        flip (EventListMB.switchTimeL errorEndOfList) xs0 $ \t xs1 ->
                        mapPair
                           (NonNegChunky.fromChunks . (t:) .
                            NonNegChunky.toChunks,
                            EventListMB.consTime t) $
                        recourse xs1)
                    _:ys -> (ys, (NonNeg.zero, xs0))
          in  mapSnd
                 (EventListMB.consBody (prefix++suffix1))
                 rest
   in  recourse

durationRemoveTB ::
   (NonNeg.C time) =>
   (body -> Bool) ->
   EventList.T time [body] ->
   (NonNegChunky.T time, EventList.T time [body])
durationRemoveTB p =
   let errorEndOfList =
          (error "no matching body element found",
           error "list ended before matching element found")
       recourse =
          EventList.switchL errorEndOfList $ \(t,evs) xs ->
          let (prefix, suffix0) = break p evs
              (suffix1, rest) =
                 case suffix0 of
                    [] -> ([], recourse xs)
                    _:ys -> (ys, (NonNeg.zero, xs))
          in  mapPair
                 (NonNegChunky.fromChunks . (t:) .
                  NonNegChunky.toChunks,
                  EventList.cons t (prefix++suffix1))
                 rest
   in  recourse


-- ToDo: move to somewhere else, this has nothing todo with event lists

makeInstrumentArray :: [instr] -> Array Program instr
makeInstrumentArray instrs =
   listArray
      (ChannelMsg.toProgram 0, ChannelMsg.toProgram (length instrs - 1))
      instrs

getInstrumentFromArray :: Array Program instr -> Program -> Program -> instr
getInstrumentFromArray bank defltPgm pgm =
   bank !
   if inRange (bounds bank) pgm
     then pgm else defltPgm
