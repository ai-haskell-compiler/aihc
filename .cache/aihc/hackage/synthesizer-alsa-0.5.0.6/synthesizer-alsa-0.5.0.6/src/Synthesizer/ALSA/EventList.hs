{-# LANGUAGE RebindableSyntax #-}
module Synthesizer.ALSA.EventList where

import qualified Sound.ALSA.Sequencer.Address as Addr
import qualified Sound.ALSA.Sequencer.Client as Client
import qualified Sound.ALSA.Sequencer.Port as Port
import qualified Sound.ALSA.Sequencer.Port.InfoMonad as PortInfo
import qualified Sound.ALSA.Sequencer.Event as Event
import qualified Sound.ALSA.Sequencer.Queue as Queue
import qualified Sound.ALSA.Sequencer.Time as Time
import qualified Sound.ALSA.Sequencer.RealTime as RealTime
import qualified Sound.ALSA.Sequencer as SndSeq
import qualified Sound.ALSA.Exception as AlsaExc

import qualified Data.EventList.Relative.TimeBody  as EventList
import qualified Data.EventList.Relative.TimeTime  as EventListTT
import qualified Data.EventList.Relative.MixedBody as EventListMB
import qualified Data.EventList.Relative.TimeMixed as EventListTM
import qualified Data.EventList.Absolute.TimeBody  as AbsEventList

import Sound.MIDI.ALSA.Construct ()
import Sound.MIDI.ALSA.Check ()
import Sound.MIDI.ALSA.Query ()

import System.IO.Unsafe (unsafeInterleaveIO, )
import Control.Concurrent (threadDelay)
import System.Time (ClockTime(TOD), getClockTime, )

import Control.Monad.Trans.State
          (evalState, modify, get, )

import qualified Numeric.NonNegative.Class   as NonNeg
import qualified Numeric.NonNegative.Wrapper as NonNegW

import qualified Algebra.RealField  as RealField
import qualified Algebra.Field      as Field

import Data.Tuple.HT (mapPair, mapSnd, )
import Data.Ord.HT (limit, )
import Control.Monad (liftM, liftM2, )

import NumericPrelude.Numeric
import NumericPrelude.Base


{- |
The @time@ type needs high precision,
so you will certainly have to instantiate it with 'Double'.
'Float' has definitely not enough bits.
-}
getTimeSeconds :: Field.C time => IO time
getTimeSeconds =
   fmap clockTimeToSeconds getClockTime

clockTimeToSeconds :: Field.C time => ClockTime -> time
clockTimeToSeconds (TOD secs picos) =
   fromInteger secs + fromInteger picos * 1e-12

wait :: RealField.C time => time -> IO ()
wait t1 =
   do t0 <- getTimeSeconds
      threadDelay $ floor $ 1e6*(t1-t0)


{-
We cannot easily turn this into a custom type,
since we need Maybe Event.T sometimes.
-}
type StampedEvent time = (time, Event.T)


{- |
only use it for non-blocking sequencers

We ignore ALSA time stamps and use the time of fetching the event,
because I don't know whether the ALSA time stamps are in sync with getClockTime.
-}
getStampedEvent ::
   (Field.C time, SndSeq.AllowInput mode) =>
   SndSeq.T mode -> IO (StampedEvent time)
getStampedEvent h =
   liftM2 (,)
      getTimeSeconds
      (Event.input h)

{- | only use it for non-blocking sequencers -}
getWaitingStampedEvents ::
   (Field.C time, SndSeq.AllowInput mode) =>
   SndSeq.T mode -> IO [StampedEvent time]
getWaitingStampedEvents h =
   let loop =
          AlsaExc.catch
             (liftM2 (:) (getStampedEvent h) loop)
             (const $ return [])
   in  loop

{- |
RealTime.toFractional for NumericPrelude.
-}
realTimeToField :: (Field.C a) => RealTime.T -> a
realTimeToField (RealTime.Cons s n) =
   fromIntegral s + fromIntegral n / (10^9)

addStamp ::
   (RealField.C time) =>
   Event.T -> StampedEvent time
addStamp ev =
   (case Event.time ev of
      Time.Cons Time.Absolute (Time.Real t) -> realTimeToField t
      _ -> error "unsupported time stamp type",
    ev)

{- | only use it for blocking sequencers -}
getStampedEventsUntilTime ::
   (RealField.C time,
    SndSeq.AllowInput mode, SndSeq.AllowOutput mode) =>
   SndSeq.T mode ->
   Queue.T -> Port.T -> time ->
   IO [StampedEvent time]
getStampedEventsUntilTime h q p t =
   fmap (map addStamp) $ getEventsUntilTime h q p t


{- |
The client id may differ from the receiving sequencer.
I do not know, whether there are circumstances, where this is useful.
-}
getEventsUntilEcho ::
   (SndSeq.AllowInput mode) =>
   Client.T -> SndSeq.T mode -> IO [Event.T]
getEventsUntilEcho c h =
   let loop = do
          ev <- Event.input h
          let abort =
                 case Event.body ev of
                    Event.CustomEv Event.Echo _ ->
                       c == Addr.client (Event.source ev)
                    _ -> False
          if abort
            then return []
            else liftM (ev:) loop
   in  loop

{- |
Get events until a certain point in time.
It sends itself an Echo event in order to measure time.
-}
getEventsUntilTime ::
   (RealField.C time,
    SndSeq.AllowInput mode, SndSeq.AllowOutput mode) =>
   SndSeq.T mode ->
   Queue.T -> Port.T -> time ->
   IO [Event.T]
getEventsUntilTime h q p t = do
   c <- Client.getId h
   _ <- Event.output h $
           makeEcho c q p t (Event.Custom 0 0 0)
   _ <- Event.drainOutput h
   getEventsUntilEcho c h


getWaitingEvents ::
   (SndSeq.AllowInput mode) =>
   SndSeq.T mode -> IO [Event.T]
getWaitingEvents h =
   let loop =
          AlsaExc.catch
             (liftM2 (:) (Event.input h) loop)
             (const $ return [])
   in  loop



type StrictTime = NonNegW.Integer
newtype ClientName = ClientName String
   deriving (Show)

{-
ghc -i:src -e 'withMIDIEvents 44100 print' src/Synthesizer/Storable/ALSA/MIDI.hs
-}
{-
Maybe it is better to not use type variable for sample rate,
because ALSA supports only integers,
and if ALSA sample rate and sample rate do not match due to rounding errors,
then play and event fetching get out of sync over the time.
-}
withMIDIEvents :: (RealField.C time) =>
   ClientName -> time -> time ->
   (EventList.T StrictTime [Event.T] -> IO a) -> IO a
withMIDIEvents =
   withMIDIEventsBlockEcho


{-
as a quick hack, we neglect the ALSA time stamp and use getTime or so
-}
withMIDIEventsNonblockWaitGrouped :: (RealField.C time) =>
   ClientName -> time -> time ->
   (EventList.T StrictTime [Event.T] -> IO a) -> IO a
withMIDIEventsNonblockWaitGrouped name beat rate proc =
   withInPort name SndSeq.Nonblock $ \ h _p ->
   do start <- getTimeSeconds
      l <- lazySequence $
              flip map (iterate (beat+) start) $ \t ->
                 wait t >>
                 liftM
                    (\evs -> (t, evs))
                    (getWaitingEvents h)
{-
                 liftM2 (,)
                    getTimeSeconds
                    (getWaitingEvents h)
-}
      proc $
         discretizeTime rate $
         AbsEventList.fromPairList l

{-
With this function latency becomes longer and longer if xruns occur,
but the latency is not just adapted,
but ones xruns occur, this implies more and more xruns.
-}
withMIDIEventsNonblockWaitDefer :: (RealField.C time) =>
   ClientName -> time -> time ->
   (EventList.T StrictTime (Maybe Event.T) -> IO a) -> IO a
withMIDIEventsNonblockWaitDefer name beat rate proc =
   withInPort name SndSeq.Nonblock $ \ h _p ->
   do start <- getTimeSeconds
      l <- lazySequence $
              flip map (iterate (beat+) start) $ \t ->
                 wait t >>
                 liftM
                    (\ es -> (t, Nothing) : map (mapSnd Just) es)
                    (getWaitingStampedEvents h)
      proc $
         discretizeTime rate $
         {-
         delay events that are in wrong order
         disadvantage: we cannot guarantee a beat with a minimal period
         -}
         flip evalState start $
         AbsEventList.mapTimeM (\t -> modify (max t) >> get) $
         AbsEventList.fromPairList $ concat l

{-
We risk and endless skipping when the beat is too short.
(Or debug output slows down processing.)
-}
withMIDIEventsNonblockWaitSkip :: (RealField.C time) =>
   ClientName -> time -> time ->
   (EventList.T StrictTime (Maybe Event.T) -> IO a) -> IO a
withMIDIEventsNonblockWaitSkip name beat rate proc =
   withInPort name SndSeq.Nonblock $ \ h _p ->
   do start <- getTimeSeconds
      l <- lazySequence $
           flip map (iterate (beat+) start) $ \t ->
              do wait t
                 t0 <- getTimeSeconds
                 -- print (t-start,t0-start)
                 es <-
                    if t0>=t+beat
                      then return []
                      else getWaitingStampedEvents h
                 return $
                    (t0, Nothing) :
                    map (mapSnd Just) es
      proc $
         discretizeTime rate $
         AbsEventList.fromPairList $ concat l

withMIDIEventsNonblockWaitMin :: (RealField.C time) =>
   ClientName -> time -> time ->
   (EventList.T StrictTime (Maybe Event.T) -> IO a) -> IO a
withMIDIEventsNonblockWaitMin name beat rate proc =
   withInPort name SndSeq.Nonblock $ \ h _p ->
   do start <- getTimeSeconds
      l <- lazySequence $
              flip map (iterate (beat+) start) $ \t ->
                 wait t >>
                 liftM
                    (\ es ->
                       (minimum $ t : map fst es, Nothing) :
                       map (mapSnd Just) es)
                    (getWaitingStampedEvents h)
{-
      mapM_ print $ EventList.toPairList $
         discretizeTime rate $
         AbsEventList.fromPairList $ concat l
      proc undefined
-}
      proc $
         discretizeTime rate $
         AbsEventList.fromPairList $ concat l

withMIDIEventsNonblockConstantPause :: (RealField.C time) =>
   ClientName -> time -> time ->
   (EventList.T StrictTime (Maybe Event.T) -> IO a) -> IO a
withMIDIEventsNonblockConstantPause name beat rate proc =
   withInPort name SndSeq.Nonblock $ \ h _p ->
   do l <- ioToLazyList $ threadDelay (round $ flip asTypeOf rate $ beat*1e6) >>
              liftM2 (:)
                 (liftM (\t->(t,Nothing)) getTimeSeconds)
                 (liftM (map (mapSnd Just)) (getWaitingStampedEvents h))
      proc $
         discretizeTime rate $
         AbsEventList.fromPairList $ concat l

withMIDIEventsNonblockSimple :: (RealField.C time) =>
   ClientName -> time -> time ->
   (EventList.T StrictTime Event.T -> IO a) -> IO a
withMIDIEventsNonblockSimple name beat rate proc =
   withInPort name SndSeq.Nonblock $ \ h _p ->
   do l <- ioToLazyList $
              threadDelay (round $ flip asTypeOf rate $ beat*1e6) >>
              getWaitingStampedEvents h
      proc $
         discretizeTime rate $
         AbsEventList.fromPairList $ concat l


setTimestamping ::
   SndSeq.T mode -> Port.T -> Queue.T -> IO ()
setTimestamping h p q =
   PortInfo.modify h p $ do
      PortInfo.setTimestamping True
      PortInfo.setTimestampReal True
      PortInfo.setTimestampQueue q

withMIDIEventsBlockEcho :: (RealField.C time) =>
   ClientName -> time -> time ->
   (EventList.T StrictTime [Event.T] -> IO a) -> IO a
withMIDIEventsBlockEcho name beat rate proc =
   withInPort name SndSeq.Block $ \ h p ->
   Queue.with h $ \ q ->
   do setTimestamping h p q
      Queue.control h q Event.QueueStart Nothing
      _ <- Event.drainOutput h

      proc .
         discretizeTime rate .
         AbsEventList.fromPairList .
         concat =<<
         (lazySequence $
          flip map (iterate (beat+) 0) $ \t ->
             let end = t+beat
             in  -- (\act -> do evs <- act; print evs; return evs) $
                 -- add a laziness break
                 fmap ((t,[]) :) $
                 fmap (map (mapPair (limit (t,end), (:[])))) $
                 getStampedEventsUntilTime h q p end)

{- |
This is like withMIDIEventsBlockEcho
but collects all events at the beginning of the beats.
This way, further processing steps may collapse
all controller events within one beat to one event.
-}
withMIDIEventsBlockEchoQuantised :: (RealField.C time) =>
   ClientName -> time -> time ->
   (EventList.T StrictTime [Event.T] -> IO a) -> IO a
withMIDIEventsBlockEchoQuantised name beat rate proc =
   withInPort name SndSeq.Block $ \ h p ->
   Queue.with h $ \ q ->
   do Queue.control h q Event.QueueStart Nothing
      _ <- Event.drainOutput h

      proc .
         discretizeTime rate .
         AbsEventList.fromPairList =<<
         (lazySequence $
          flip map (iterate (beat+) 0) $ \t ->
            liftM
               (\evs -> (t, evs))
               (getEventsUntilTime h q p (t+beat)))

{- |
Make sure, that @beat@ is an integer multiple of @recip rate@.
Since we round time within each chunk,
we would otherwise accumulate rounding errors over time.
-}
withMIDIEventsChunked ::
   (RealField.C time) =>
   ClientName -> time -> time ->
   ([IO (EventListTT.T StrictTime [Event.T])] -> IO a) ->
   IO a
withMIDIEventsChunked name beat rate proc =
   withInPort name SndSeq.Block $ \ h p ->
   Queue.with h $ \ q ->
   do setTimestamping h p q
      Queue.control h q Event.QueueStart Nothing
      _ <- Event.drainOutput h

      proc $
         map
            (\t ->
               let end = t+beat
               in  liftM
                      (\evs ->
                         EventListTM.switchBodyR
                            (error "withMIDIEventsChunked: empty list, but there must be at least the end event")
                            const $
                         discretizeTime rate $
                         AbsEventList.fromPairList $
                         (t,[]) :
                         {-
                         FIXME: This is a quick hack in order to assert
                         that all events are within one chunk
                         and do not lie on the boundary.
                         -}
                         map (mapPair (limit (t , end - recip rate), (:[]))) evs ++
                         (end, []) :
                         [])
                      (getStampedEventsUntilTime h q p end))
            (iterate (beat+) 0)

withMIDIEventsChunkedQuantised ::
   (RealField.C time) =>
   ClientName -> time -> time ->
   ([IO (EventList.T StrictTime [Event.T])] -> IO a) ->
   IO a
withMIDIEventsChunkedQuantised name beat rate proc =
   withInPort name SndSeq.Block $ \ h p ->
   Queue.with h $ \ q ->
   do Queue.control h q Event.QueueStart Nothing
      _ <- Event.drainOutput h

      proc $
         map
            (\t ->
               liftM
                  (\evs ->
                     EventList.cons NonNeg.zero evs $
                     EventList.singleton
                        (NonNegW.fromNumberMsg "chunked time conversion" $
                         round (beat*rate)) [])
                  (getEventsUntilTime h q p (t+beat)))
            (iterate (beat+) 0)

makeEcho ::
   RealField.C time =>
   Client.T -> Queue.T -> Port.T ->
   time -> Event.Custom -> Event.T
makeEcho c q p t dat =
   (Event.simple
      (Addr.Cons {
           Addr.client = c,
           Addr.port = Port.unknown
        })
      (Event.CustomEv Event.Echo dat))
      { Event.queue = q
      , Event.time =
           Time.consAbs $ Time.Real $ RealTime.fromInteger $
           floor (10^9 * t)
      , Event.dest = Addr.Cons {
           Addr.client = c,
           Addr.port = p
        }
      }

withMIDIEventsBlock :: (RealField.C time) =>
   ClientName -> time ->
   (EventList.T StrictTime Event.T -> IO a) -> IO a
withMIDIEventsBlock name rate proc =
   withInPort name SndSeq.Block $ \ h _p ->
   do l <- ioToLazyList $ getStampedEvent h
      proc $
         discretizeTime rate $
         AbsEventList.fromPairList l

withInPort ::
   ClientName ->
   SndSeq.BlockMode ->
   (SndSeq.T SndSeq.DuplexMode -> Port.T -> IO t) -> IO t
withInPort (ClientName name) blockMode act =
   SndSeq.with SndSeq.defaultName blockMode $ \h ->
   Client.setName h name >>
   Port.withSimple h "input"
      (Port.caps [Port.capWrite, Port.capSubsWrite])
      Port.typeMidiGeneric
      (act h)

{- |
We first discretize the absolute time values,
then we compute differences,
in order to avoid rounding errors in further computations.
-}
discretizeTime :: (RealField.C time) =>
   time -> AbsEventList.T time a -> EventList.T StrictTime a
discretizeTime sampleRate =
   EventListMB.mapTimeHead (const $ NonNegW.fromNumber zero) . -- clear first time since it is an absolute system time stamp
   EventList.fromAbsoluteEventList .
   AbsEventList.mapTime
      (NonNegW.fromNumberMsg "time conversion" . round . (sampleRate*))



ioToLazyList :: IO a -> IO [a]
ioToLazyList m =
   let go = unsafeInterleaveIO $ liftM2 (:) m go
   in  go

lazySequence :: [IO a] -> IO [a]
lazySequence [] = return []
lazySequence (m:ms) =
   unsafeInterleaveIO $ liftM2 (:) m $ lazySequence ms
