module Reactive.Banana.MIDI.Process (
   RelativeTicks,
   AbsoluteTicks,
   RelativeSeconds,
   MomentIO(liftMomentIO),
   Reactor(reserveSchedule),
   scheduleQueue,
   initialEvent,
   beat,
   beatQuant,
   beatVar,
   delaySchedule,
   delay,
   delayAdd,
   pressed,
   latch,
   controllerRaw,
   controllerExponential,
   controllerLinear,
   tempoCtrl,
   snapSelect,
   uniqueChanges,
   sweep,
   makeControllerLinear,
   cyclePrograms,
   cycleProgramsDefer,
   noteSequence,
   guitar,
   trainer,
   ) where

import qualified Reactive.Banana.MIDI.Guitar as Guitar
import qualified Reactive.Banana.MIDI.Program as Program
import qualified Reactive.Banana.MIDI.Controller as Ctrl
import qualified Reactive.Banana.MIDI.Note as Note
import qualified Reactive.Banana.MIDI.Time as Time
import qualified Reactive.Banana.MIDI.KeySet as KeySet
import qualified Reactive.Banana.MIDI.Pitch as Pitch
import qualified Reactive.Banana.MIDI.Utility as RBU
import qualified Reactive.Banana.MIDI.Common as Common
import Reactive.Banana.MIDI.Common
          (PitchChannel(PitchChannel),
           PitchChannelVelocity(PitchChannelVelocity),
           fraction, )

import qualified Reactive.Banana.Bunch.Combinators as RB
import qualified Reactive.Banana.Bunch.Frameworks as RBF
import Reactive.Banana.Bunch.Combinators ((<@>), )

import qualified Sound.MIDI.Message.Class.Construct as Construct
import qualified Sound.MIDI.Message.Class.Check as Check
import qualified Sound.MIDI.Message.Class.Query as Query
import Sound.MIDI.Message.Channel (Channel, )
import Sound.MIDI.Message.Channel.Voice
          (Pitch, Velocity, Controller, Program, fromPitch, )

import qualified Data.EventList.Relative.TimeBody as EventList
import qualified Data.EventList.Absolute.TimeBody as EventListAbs

import qualified Data.Accessor.Monad.Trans.State as AccState
import qualified Data.Accessor.Tuple as AccTuple

import qualified Control.Monad.Trans.State as MS

import qualified Data.Traversable as Trav
import Control.Monad (join, mplus, when, liftM, )
import Control.Applicative (pure, liftA2, (<*>), (<$>), )
import Data.Monoid (mempty, mappend, )
import Data.Tuple.HT (mapPair, mapSnd, )
import Data.Ord.HT (comparing, limit, )
import Data.Maybe.HT (toMaybe, )
import Data.Maybe (catMaybes, )

import qualified Data.Map as Map
import qualified Data.List.Key as Key
import qualified Data.List.Match as Match
import qualified Data.List as List

import Prelude hiding (sequence, )


type RelativeTicks   m = Time.T m Time.Relative Time.Ticks
type AbsoluteTicks   m = Time.T m Time.Absolute Time.Ticks
type RelativeSeconds m = Time.T m Time.Relative Time.Seconds

class MomentIO moment where
   liftMomentIO :: RBF.MomentIO a -> moment a

instance MomentIO RBF.MomentIO where
   liftMomentIO = id


class (MomentIO reactor, Time.Timed reactor) => Reactor reactor where
   {- |
   Provide a function for registering future beats
   and return the reactive event list that results from the sent beats.
   -}
   reserveSchedule ::
      reactor
         ([AbsoluteTicks reactor] -> IO (), IO (),
          RB.Event (AbsoluteTicks reactor))

reactimate ::
   (MomentIO reactor) =>
   RB.Event (IO ()) -> reactor ()
reactimate = liftMomentIO . RBF.reactimate

reactimate' ::
   (MomentIO reactor) =>
   RB.Event (RBF.Future (IO ())) -> reactor ()
reactimate' = liftMomentIO . RBF.reactimate'

liftIO :: (MomentIO m) => IO a -> m a
liftIO = liftMomentIO . RBF.liftIO



scheduleQueue ::
   (Reactor reactor) =>
   RB.Behavior (AbsoluteTicks reactor) ->
   RB.Event (Common.Bundle reactor a) -> reactor (RB.Event a)
scheduleQueue times e = do
   (send, _cancel, eEcho) <- reserveSchedule
   let -- maintain queue and generate Echo events
       remove echoTime =
          MS.state $ uncurry $ \_lastTime ->
          EventList.switchL
             (error "scheduleQueue: received more events than sent")
             (\(_t,x) xs ->
                ((Just x, return () {- "got echo for event: " ++ show x -}),
                 ({- Time.inc t lastTime -}
                  echoTime, xs)))
       add time new = do
          MS.modify $ \(lastTime, old) ->
             (time,
              Common.mergeStable
                 (EventList.fromAbsoluteEventListGen Time.subSat mempty $
                  EventListAbs.fromPairList $
                  map (\(Common.Future dt a) -> (dt, a)) $
                  List.sortBy (comparing Common.futureTime) new) $
              EventList.decreaseStart
                 (Time.subSat time lastTime) old)
          return (Nothing, send $ map (flip Time.inc time . Common.futureTime) new)

   -- (Queue that keeps track of events to schedule
   -- , duration of the new alarm if applicable)
   (eEchoEvent, _bQueue) <-
      RBU.sequence (mempty, EventList.empty) $
      RB.union (fmap remove eEcho) (add <$> times <@> e)

   reactimate $ fmap snd eEchoEvent
   return $ RBU.mapMaybe fst eEchoEvent



{- |
Generate an event at the first time point.
-}
initialEvent ::
   (Reactor reactor) =>
   a -> reactor (RB.Event a)
initialEvent x = do
   (send, _cancel, eEcho) <- reserveSchedule
   liftIO $ send [mempty]
   return $ fmap (const x) eEcho


{- |
Generate a beat according to the tempo control.
The input signal specifies the period between two beats.
The output events hold the times, where they occur.
-}
beat ::
   (Reactor reactor) =>
   RB.Behavior (RelativeTicks reactor) ->
   reactor (RB.Event (AbsoluteTicks reactor))
beat tempo = do
   (send, _cancel, eEcho) <- reserveSchedule

   liftIO $ send [mempty]

   let next dt time = (time, send [Time.inc dt time])
       eEchoEvent = fmap next tempo <@> eEcho

   reactimate $ fmap snd eEchoEvent
   return $ fmap fst eEchoEvent


{- |
Similar to 'beat' but warrants a maximum reaction time to tempo changes.
This way you can alter slow tempos to faster one more quickly.
-}
{-
Instead of this we could use the reciprocal of Time, that is frequency,
and integrate that.
But integration of a piecewise RBU.constant function means a linear function.
This cannot be represented in FRP.
The approach we use here samples the tempo signal
and thus may miss some tempo changes.
-}
beatQuant ::
   (Reactor reactor) =>
   RelativeTicks reactor ->
   RB.Behavior (RelativeTicks reactor) ->
   reactor (RB.Event (AbsoluteTicks reactor))
beatQuant maxDur tempo = do
   (send, _cancel, eEcho) <- reserveSchedule

   liftIO $ send [mempty]

   let next dt time = do
          complete <- MS.gets (>=1)
          when complete $ MS.modify (subtract 1)
          portion <- MS.get
          let dur = limit (mempty,maxDur) (Time.scaleCeiling (1-portion) dt)
          MS.modify (Time.div dur dt +)
          return
             (toMaybe complete time,
              send [Time.inc dur time]
              {- print (dur, time, dt, portion) -} )

   eEchoEvent <- liftM fst $ RBU.sequence 0 $ fmap next tempo <@> eEcho

   reactimate $ fmap snd eEchoEvent
   return $ RBU.mapMaybe fst eEchoEvent


beatVarNext ::
   AbsoluteTicks reactor ->
   MS.State
      (AbsoluteTicks reactor, Double, RelativeTicks reactor)
      (Maybe (AbsoluteTicks reactor), AbsoluteTicks reactor)
beatVarNext _t = do
   (t0,r,p) <- MS.get
   {-
   It should be t1==t,
   where t is the timestamp from an Echo message
   and t1 is the computed time.
   In principle we could use t,
   but this will be slightly later than the reference time t1.
   -}
   let t1 = Time.inc (Time.scale r p) t0
   MS.put (t1,1,p)
   return (Just t1, Time.inc p t1)

beatVarChange ::
   RelativeTicks reactor -> AbsoluteTicks reactor ->
   MS.State
      (AbsoluteTicks reactor, Double, RelativeTicks reactor)
      (AbsoluteTicks reactor)
beatVarChange p1 t1 = do
   (t0,r0,p0) <- MS.get
   let r1 = max 0 $ r0 - Time.div (Time.subSat t1 t0) p0
   MS.put (t1,r1,p1)
   return (Time.inc (Time.scale r1 p1) t1)

{- |
Similar to 'beat' but it reacts immediately to tempo changes.
This requires the ability of the backend (e.g. ALSA)
to cancel sent (Echo) messages
and it requires to know the precise time points of tempo changes,
thus we need the Discrete input instead of Behaviour
and we need a behaviour for the current time.
-}
{-
TODO: However, the best solution specifically for ALSA would be
to reserve a queue for every beat
and alter the tempo of the queue timer.
-}
beatVar ::
   (Reactor reactor) =>
   RB.Behavior (AbsoluteTicks reactor) ->
   RB.Behavior (RelativeTicks reactor) ->
   reactor (RB.Event (AbsoluteTicks reactor))
beatVar time tempo = do
   (send, cancel, eEcho) <- reserveSchedule
   let sendSingle = send . (:[])

   liftIO $ sendSingle mempty

   (tempoInit, tempoChanges) <-
      liftMomentIO $
      liftA2 (,) (RB.valueBLater tempo) (RBF.plainChanges tempo)

   let next t = mapSnd (return . sendSingle) <$> beatVarNext t

       change p1 t1 = do
          ta <- beatVarChange p1 t1
          return (Nothing, return $ cancel >> sendSingle ta)

   eEchoEvent <-
      liftM fst $ RBU.sequence (mempty, 0, tempoInit) $
      RB.union (next <$> eEcho) (flip change <$> time <@> tempoChanges)

   reactimate' $ fmap snd eEchoEvent
   return $ RBU.mapMaybe fst eEchoEvent


{- |
Demonstration of scheduleQueue.
For real use with ALSA you should prefer 'delay',
since this uses precisely timed delivery by ALSA.
-}
delaySchedule ::
   (Reactor reactor) =>
   RelativeTicks reactor ->
   RB.Behavior (AbsoluteTicks reactor) ->
   RB.Event a -> reactor (RB.Event a)
delaySchedule dt times =
   scheduleQueue times . fmap ((:[]) . Common.Future dt)


delay ::
   RelativeTicks m ->
   RB.Event ev -> RB.Event (Common.Future m ev)
delay dt =
   fmap (Common.Future dt)

delayAdd ::
   RelativeTicks m ->
   RB.Event ev -> RB.Event (Common.Future m ev)
delayAdd dt evs =
   RB.union (fmap Common.now evs) $ delay dt evs


{- |
register pressed keys
-}
pressed ::
   (RB.MonadMoment m, KeySet.C set, Ord key) =>
   set key value ->
   RB.Event (Note.BoundaryExt key value) ->
   m (RB.Event [Note.Boundary key value], RB.Behavior (set key value))
pressed empty =
   RBU.traverse empty KeySet.changeExt

latch ::
   (RB.MonadMoment m, Ord key) =>
   RB.Event (Note.Boundary key value) ->
   m (RB.Event (Note.Boundary key value),
      RB.Behavior (Map.Map key value))
latch =
   liftM (mapPair (RB.filterJust, fmap KeySet.deconsLatch)) .
   RBU.traverse KeySet.latch KeySet.latchChange


controllerRaw ::
   (RB.MonadMoment m, Check.C ev) =>
   Channel ->
   Controller ->
   Int ->
   RB.Event ev -> m (RB.Behavior Int)
controllerRaw chan ctrl deflt =
   RB.stepper deflt . RBU.mapMaybe (Check.controller chan ctrl)

controllerExponential ::
   (RB.MonadMoment m, Floating a, Check.C ev) =>
   Channel ->
   Controller ->
   a -> (a,a) ->
   RB.Event ev -> m (RB.Behavior a)
controllerExponential chan ctrl deflt (lower,upper) =
   let k = log (upper/lower) / 127
   in  RB.stepper deflt .
       RBU.mapMaybe
          (fmap ((lower*) . exp . (k*) . fromIntegral)
              . Check.controller chan ctrl)

controllerLinear ::
   (RB.MonadMoment m, Fractional a, Check.C ev) =>
   Channel ->
   Controller ->
   a -> (a,a) ->
   RB.Event ev -> m (RB.Behavior a)
controllerLinear chan ctrl deflt (lower,upper) =
   let k = (upper-lower) / 127
   in  RB.stepper deflt .
       RBU.mapMaybe
          (fmap ((lower+) . (k*) . fromIntegral)
              . Check.controller chan ctrl)


-- | FuncHT.mapFst
mapFstM :: Monad m => (a -> m c) -> (a, b) -> m (c, b)
mapFstM f ~(a,b) = liftM (flip (,) b) $ f a

tempoCtrl ::
   (RB.MonadMoment m, Check.C ev) =>
   Channel ->
   Controller ->
   RelativeTicks m ->
   (RelativeTicks m, RelativeTicks m) ->
   RB.Event ev ->
   m (RB.Behavior (RelativeTicks m), RB.Event ev)
tempoCtrl chan ctrl deflt (lower,upper) =
   mapFstM (RB.stepper deflt) .
   RBU.partitionMaybe
      (fmap (Ctrl.duration (lower, upper))
          . Check.controller chan ctrl)


{- |
Use a MIDI controller for selecting a note from a key set.
Only the pitch class of the keys is respected.
The controller behavior must be in the range 0-127.
This way, it accesses the whole range of MIDI notes.
The output note is stopped and a new note is played
whenever turning the knob alters the note pitch.
The advantage of the effect is that the pitch range of the knob
does not depend on the number of pressed keys.
The disadvantage is that there are distinct distances between the pitches.
-}
snapSelect ::
   (MomentIO moment, KeySet.C set, Pitch.C pitch, Eq pitch, Eq value) =>
   RB.Behavior (set pitch value) ->
   RB.Behavior Int ->
   moment (RB.Event [Note.Boundary pitch value])
snapSelect set ctrl =
   liftMomentIO $
   (flip RBU.mapAdjacent Nothing
         (\oldNote newNote ->
            let note on (pc, v) = Note.Boundary pc v on
            in  catMaybes [fmap (note False) oldNote,
                           fmap (note True) newNote]) =<<) $
   uniqueChanges $
   liftA2
      (\s x ->
         toMaybe (not $ null s) $
         Key.minimum (\(pc, _v) -> abs (fromPitch (Pitch.extract pc) - x)) $
         map (\(pc, v) -> (Pitch.toClosestOctave x pc, v)) s)
      (fmap KeySet.toList set) ctrl


{-
TODO:
I think plainChanges works for ALSA.
Can we also use it for JACK?
If not, we can create something of type

  RB.Behavior a -> RB.Moment (RB.Event ())

and attach the Behavior values using (<@).
-}
uniqueChanges ::
   (MomentIO moment, Eq a) => RB.Behavior a -> moment (RB.Event a)
uniqueChanges x = liftMomentIO $ do
   x0 <- RB.valueBLater x
   xs <- RBF.plainChanges x
   fmap RB.filterJust $
      flip RBU.mapAdjacent x0 (\old new -> toMaybe (new/=old) new) xs
--   return xs
{-
   return $ RB.filterJust $ fst $
      RB.mapAccum x0 $ fmap (\new old -> (toMaybe (new/=old) new, new)) xs
-}


sweep ::
   (Reactor reactor) =>
   RelativeSeconds reactor ->
   (Double -> Double) ->
   RB.Behavior Double ->
   reactor
      (RB.Event (AbsoluteTicks reactor),
       RB.Behavior Double)
sweep durSecs wave speed = do
   bt <- beat . pure =<< Time.ticksFromSeconds durSecs
   let dur = realToFrac $ Time.unSeconds $ Time.decons durSecs
   phases <-
      RB.accumB 0 $
      fmap (\d _ phase -> fraction (phase + dur * d)) speed <@> bt
   return (bt, fmap wave phases)

makeControllerLinear ::
   (Construct.C msg) =>
   Channel -> Controller ->
   RB.Behavior Int ->
   RB.Behavior Int ->
   RB.Event time -> RB.Behavior Double ->
   RB.Event msg
makeControllerLinear chan cc depthCtrl centerCtrl bt ctrl =
   pure
      (\y depth center _time ->
         curry (Construct.anyController chan) cc $
         round $ limit (0,127) $
         fromIntegral center + fromIntegral depth * y)
      <*> ctrl
      <*> depthCtrl
      <*> centerCtrl
      <@> bt



cyclePrograms ::
   (RB.MonadMoment m, Construct.C msg, Query.C msg) =>
   [Program] ->
   RB.Event msg -> m (RB.Event (Maybe msg))
cyclePrograms pgms =
   liftM fst . RBU.traverse (cycle pgms) (Program.traverseSeek (length pgms))


{- |
> cycleProgramsDefer t

After a note that triggers a program change,
we won't change the program in the next 't' seconds.
This is in order to allow chords being played
and in order to skip accidentally played notes.
-}
{-
In the future we might also add a time-out:
After a certain time, where no key is pressed,
the program would be reset to the initial program.
-}
cycleProgramsDefer ::
   (RB.MonadMoment m, Construct.C msg, Query.C msg) =>
   RelativeTicks m -> [Program] ->
   RB.Behavior (AbsoluteTicks m) ->
   RB.Event msg -> m (RB.Event (Maybe msg))
cycleProgramsDefer defer pgms times =
   liftM fst .
   RBU.traverse (cycle pgms, mempty)
      (\(eventTime,e) ->
         fmap join $ Trav.sequence $
         mplus
            (flip fmap (Query.program e) $ \(_chan, pgm) ->
               AccState.lift AccTuple.first $
                  Program.seek (length pgms) pgm)
            (flip fmap (Program.maybeNoteOn e) $ \chan -> do
               blockTime <- MS.gets snd
               if eventTime < blockTime
                 then return Nothing
                 else do
                    AccState.set AccTuple.second $
                       Time.inc defer eventTime
                    AccState.lift AccTuple.first $
                       Program.next chan)) .
   RB.apply (fmap (,) times)


noteSequence ::
   RelativeTicks m ->
   Bool -> [Bool -> msg] ->
   Common.Bundle m msg
noteSequence stepTime on =
   zipWith Common.Future (iterate (mappend stepTime) mempty) . map ($on)

{- |
This process simulates playing chords on a guitar.
If you press some keys like C, E, G on the keyboard,
then this process figures out what tones would be played on a guitar.

Call it like @guitar stepTime chords triggers@.

@stepTime@ is the delay between to successive notes.
A good value is 0.03 (seconds).
The chords to be played are passed in by @chords@.
This should be the output of 'pressed'.
Further on the function needs events
that trigger playing the chord in @trigger@ argument.
The trigger consists of the trigger time
and the direction to be played
('True' = down from high to low pitches,
'False' = up from low to high pitches).
The trigger may be derived from a specific key that is pressed and released,
or two keys, one for each direction.
-}
guitar ::
   (RB.MonadMoment m, Construct.C msg, KeySet.C set) =>
   RelativeTicks m ->
   RB.Behavior (set PitchChannel Velocity) ->
   RB.Event Bool ->
   m (RB.Event (Common.Bundle m msg))
guitar stepTime pressd trigger =
   liftM fst $
   RBU.traverse []
      (\(set, on) -> do
         played <- MS.get
         let toPlay =
                case KeySet.toList set of
                   [] -> []
                   list ->
                      fmap (\(PitchChannelVelocity pc v) -> Note.make pc v) $
                      Guitar.mapChordToString Guitar.stringPitches $
                      fmap (uncurry PitchChannelVelocity) list
         MS.put toPlay
         return $
            if on
              then
                 noteSequence stepTime False
                    (List.reverse played)
                 ++
                 noteSequence stepTime True toPlay
              else
                 noteSequence stepTime False played
                 ++
                 noteSequence stepTime True
                    (List.reverse toPlay)) $
   (,) <$> pressd <@> trigger



{- |
Audio perception trainer

Play sets of notes and
let the human player answer to them according to a given scheme.
Repeat playing the notes sets until the trainee answers correctly.
Then continue with other sequences, maybe more complicated ones.

possible tasks:

 - replay a sequence of pitches on the keyboard:
      single notes for training abolute pitches,
      intervals all with the same base notes,
      intervals with different base notes

 - transpose a set of pitches:
      tranpose to a certain base note,
      transpose by a certain interval

 - play a set of pitches in a different order:
      reversed order,
      in increasing pitch

 - replay a set of simultaneously pressed keys

The difficulty can be increased by not connecting
the keyboard directly with the sound generator.
This way, the trainee cannot verify,
how the pressed keys differ from the target keys.

Sometimes it seems that you are catched in an infinite loop.
This happens if there were too many keys pressed.
The trainer collects all key press events,
not only the ones that occur after the target set is played.
This way you can correct yourself immediately,
before the target is repeatedly played.
The downside is, that there may be key press events hanging around.
You can get rid of them by pressing a key again and again,
but slowly, until the target is played, again.
Then the queue of registered keys should be empty
and you can proceed training.
-}
{-
The Reactor monad is only needed for sending the initial notes.
-}
trainer ::
   (Reactor reactor,
    Query.C msg, Construct.C msg, Time.Quantity time) =>
   Channel ->
   Time.T reactor Time.Relative time ->
   Time.T reactor Time.Relative time ->
   [([Pitch], [Pitch])] ->
   RB.Behavior (AbsoluteTicks reactor) ->
   RB.Event msg ->
   reactor (RB.Event (Common.Bundle reactor msg))
trainer chan pauseSecs durationSecs sets0 times evs0 = do
   pause    <- Time.ticksFromAny pauseSecs
   duration <- Time.ticksFromAny durationSecs
   let makeSeq sets =
          case sets of
             (target, _) : _ ->
                (concat $
                 zipWith
                    (\t p ->
                       Note.bundle t duration
                          (PitchChannel p chan, Common.normalVelocity))
                    (iterate (mappend duration) pause) target,
                 mappend pause $ Time.scaleInt (length target) duration)
             [] -> ([], mempty)

   let (initial, initIgnoreUntil) = makeSeq sets0
   initEv <- initialEvent initial

   liftM (RB.union initEv . fst) $
      flip (RBU.traverse (sets0, [], Time.inc initIgnoreUntil mempty))
         (fmap (,) times <@> evs0) $ \(time,ev) ->
      case Query.noteExplicitOff ev of
         Just (_chan, (_vel, pitch, True)) -> do
            ignoreUntil <- AccState.get AccTuple.third3
            if time <= ignoreUntil
              then return []
              else do
                 pressd <- AccState.get AccTuple.second3
                 let newPressd = pitch : pressd
                 AccState.set AccTuple.second3 newPressd
                 sets <- AccState.get AccTuple.first3
                 case sets of
                    (_, target) : rest ->
                       if Match.lessOrEqualLength target newPressd
                         then do
                            AccState.set AccTuple.second3 []
                            when (newPressd == List.reverse target) $
                               AccState.set AccTuple.first3 rest
                            (notes, newIgnoreUntil) <-
                               fmap makeSeq $
                               AccState.get AccTuple.first3
                            AccState.set AccTuple.third3 $
                               Time.inc newIgnoreUntil time
                            return notes
                         else return []
                    _ -> return []
         _ -> return []
