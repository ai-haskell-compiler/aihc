import qualified Sound.ALSA.PCM.Node.ALSA as PCM
import qualified Sound.ALSA.PCM.Parameters.Software as SwParam
import qualified Sound.ALSA.PCM.Parameters.Hardware as HwParam

import qualified Sound.ALSA.PCM.Debug as Debug

import qualified Sound.ALSA.Sequencer.Address as Addr
import qualified Sound.ALSA.Sequencer.Client as Client
import qualified Sound.ALSA.Sequencer.Port as Port
import qualified Sound.ALSA.Sequencer.Port.InfoMonad as PortInfo
import qualified Sound.ALSA.Sequencer.Event as Event
import qualified Sound.ALSA.Sequencer.Queue as Queue
import qualified Sound.ALSA.Sequencer.Time as Time
import qualified Sound.ALSA.Sequencer.RealTime as RealTime
import qualified Sound.ALSA.Sequencer as SndSeq
import Sound.ALSA.Sequencer.Event (Pitch, Velocity, )

import qualified Data.StorableVector.ST.Strict as SVST
import qualified Data.StorableVector as SV
import qualified Data.StorableVector.Base as SVB

import Foreign.Storable (Storable, )
import Control.Monad.ST.Strict as ST

import qualified Control.Monad.Trans.State.Strict as MS
import Control.Monad.IO.Class (liftIO, )

import qualified Data.Map as Map

import Control.Exception (bracket, )
import Control.Monad (liftM, forever, )

import Debug.Trace (trace, )


openPCM ::
   (PCM.Access i, PCM.SampleFmt y) =>
   IO (PCM.Size, PCM.SampleFreq, PCM.Handle i y)
openPCM = do
   Debug.put "alsaOpenTest"
   (((bufferSize,periodSize),(bufferTime,periodTime),sampleRate), h) <-
      PCM.open (PCM.modes []) PCM.StreamPlayback
         (setHwParams 44100 1024 64)
         (\q@(sizes,_,_) -> do
             uncurry SwParam.setBufferSize sizes
             return q)
         "default"
   PCM.prepare h
   Debug.put $ "bufferTime = " ++ show bufferTime
   Debug.put $ "bufferSize = " ++ show bufferSize
   Debug.put $ "periodTime = " ++ show periodTime
   Debug.put $ "periodSize = " ++ show periodSize
   return (periodSize, sampleRate, h)

closePCM :: (PCM.Size, PCM.SampleFreq, PCM.Handle i y) -> IO ()
closePCM (_,_,pcm) = do
   Debug.put "alsaClose"
   PCM.drain pcm
   PCM.close pcm

setHwParams ::
      PCM.SampleFreq -- ^ sample frequency
   -> PCM.Size -- ^ buffer size
   -> PCM.Size -- ^ period size
   -> HwParam.T i y ((PCM.Size,PCM.Size),(PCM.Time,PCM.Time),PCM.SampleFreq)
      -- ^ ((bufferSize,periodSize),(bufferTime,periodTime),sampleRate)
setHwParams rate bufferSize periodSize = do
{-
   (actualRate,ord) <- HwParam.getRateMax
   print ord
-}
   HwParam.setRateResample False
   (actualRate,_) <-
      HwParam.setRateNear rate EQ
   (actualPeriodSize,_) <-
      HwParam.setPeriodSizeNear periodSize EQ
   actualBufferSize <-
      HwParam.setBufferSizeNear
         (max bufferSize (actualPeriodSize*2))
{-
   let actualBufferSize = bufferSize
   HwParam.setBufferSize bufferSize
-}
   (actualBufferTime,_) <- HwParam.getBufferTime
   (actualPeriodTime,_) <- HwParam.getPeriodTime
   return ((actualBufferSize, actualPeriodSize),
           (actualBufferTime, actualPeriodTime),
           actualRate)


setTimestamping ::
   SndSeq.T mode -> Port.T -> Queue.T -> IO ()
setTimestamping h p q =
   PortInfo.modify h p $ do
      PortInfo.setTimestamping True
      PortInfo.setTimestampReal True
      PortInfo.setTimestampQueue q


withInPort ::
   SndSeq.BlockMode ->
   (SndSeq.T SndSeq.DuplexMode -> Port.T -> IO t) -> IO t
withInPort blockMode act =
   SndSeq.with SndSeq.defaultName blockMode $ \h ->
   Client.setName h "alsa-haskell-minisynth" >>
   Port.withSimple h "input"
      (Port.caps [Port.capWrite, Port.capSubsWrite])
      Port.typeApplication
      (act h)


type StampedEvent = (Time, Event.T)

type Time = Integer


{- |
RealTime.toFractional for NumericPrelude.

realTimeToField :: (Fractional a) => RealTime.T -> a
realTimeToField (RealTime.Cons s n) =
   fromIntegral s + fromIntegral n / (10^(9::Int))
-}

addStamp ::
   Time -> Event.T -> StampedEvent
addStamp rate ev =
   (case Event.time ev of
      Time.Cons Time.Absolute (Time.Real t) ->
         div (RealTime.toInteger t * rate) nano
      _ -> error "unsupported time stamp type",
    ev)


nano :: Integer
nano = 10^(9::Int)

{- | only use it for blocking sequencers -}
getStampedEventsUntilTime ::
   (SndSeq.AllowInput mode, SndSeq.AllowOutput mode) =>
   SndSeq.T mode ->
   Queue.T -> Port.T ->
   Time -> Time ->
   IO [StampedEvent]
getStampedEventsUntilTime h q p r t =
   fmap (map (addStamp r)) $ getEventsUntilTime h q p r t


{- |
Get events until a certain point in time.
It sends itself an Echo event in order to measure time.
-}
getEventsUntilTime ::
   (SndSeq.AllowInput mode, SndSeq.AllowOutput mode) =>
   SndSeq.T mode ->
   Queue.T -> Port.T ->
   Time -> Time ->
   IO [Event.T]
getEventsUntilTime h q p r t = do
--   putStrLn $ "schedule echo for " ++ show (milliseconds t)
   c <- Client.getId h
   _ <- Event.output h $
           makeEcho c q p
              (RealTime.fromInteger $ div (t * nano) r)
              (Event.Custom 0 0 0)
   _ <- Event.drainOutput h
   getEventsUntilEcho c h


makeEcho ::
   Client.T -> Queue.T -> Port.T ->
   RealTime.T -> Event.Custom -> Event.T
makeEcho c q p t dat =
   (Event.simple
      (Addr.Cons c Port.unknown)
      (Event.CustomEv Event.Echo dat))
      { Event.queue = q
      , Event.time = Time.consAbs $ Time.Real t
      , Event.dest = Addr.Cons {
           Addr.client = c,
           Addr.port = p
        }
      }


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
            then
               case Event.time ev of
                 Time.Cons Time.Absolute (Time.Real _t) -> do
--                    putStrLn $ "got Echo at: " ++ show (RealTime.toInteger t :: Double)
                    return []
                 _ -> error "unsupported time stamp type"
            else liftM (ev:) loop
   in  loop


check :: Monad m => Bool -> String -> m () -> m ()
check b msg act =
   if not b
     then trace msg $ return ()
     else act

unsafeAddChunkToBuffer :: (Storable a, Num a) =>
   SVST.Vector s a -> Int -> SV.Vector a -> ST s ()
unsafeAddChunkToBuffer v start xs =
   let go i j =
          if j >= SV.length xs
            then return ()
            else
              SVST.unsafeModify v i (SV.index xs j +) >>
              go (i + 1) (j + 1)
   in  check (start>=0)
                ("start negative: " ++ show (start, SV.length xs)) $
       check (start <= SVST.length v)
                ("start too late: " ++ show (start, SV.length xs)) $
       check (start+SV.length xs <= SVST.length v)
                ("end too late: " ++ show (start, SV.length xs)) $
       go start 0

arrange ::
   (Storable a, Num a) =>
   PCM.Size ->
   [(Int, SV.Vector a)] ->
   SV.Vector a
arrange size evs =
   SVST.runSTVector (do
      v <- SVST.new (fromIntegral size) 0
      mapM_ (uncurry $ unsafeAddChunkToBuffer v) evs
      return v)


data OscillatorState a = OscillatorState a a Int

{-
type ToneSequence a =
        (Maybe (Int, OscillatorState a),
         [(Int, Int, OscillatorState a)])

startTone :: ToneSequence a -> ToneSequence a
-}

stopTone ::
   Int ->
   (Maybe (Int, OscillatorState a),
    [(Int, Int, OscillatorState a)]) ->
   [(Int, Int, OscillatorState a)]
stopTone stopTime (mplaying, finished) =
   case mplaying of
      Just (startTime, osci) ->
         (startTime, stopTime-startTime, osci) : finished
      Nothing -> finished

renderTone ::
   (Storable a, Floating a) =>
   Int -> OscillatorState a ->
   (SV.Vector a, OscillatorState a)
renderTone dur state@(OscillatorState amp freq phase) =
   if dur<0
     then
       trace ("renderTone: negative duration " ++ show dur) $
       (SV.empty, state)
     else
       let gain = 0.9999
       in  (SV.zipWith (\y k -> y * sin (2*pi*fromIntegral k * freq))
               (SV.iterateN dur (gain*) amp)
               (SV.iterateN dur (1+) phase),
            OscillatorState (amp*gain^dur) freq (phase+dur))

amplitudeFromVelocity ::
   (Floating y) =>
   Velocity -> y
amplitudeFromVelocity (Event.Velocity vel) =
   4 ** ((fromIntegral vel - 64) / 128)

frequencyFromPitch ::
   (Floating y) =>
   Pitch -> y
frequencyFromPitch (Event.Pitch pitch) =
   440 * 2 ** (fromIntegral (fromIntegral pitch + 3 - 6*12 :: Int) / 12)

normalizeNote :: Event.NoteEv -> Event.Note -> (Event.NoteEv, Velocity)
normalizeNote notePart note =
   case Event.noteVelocity note of
      velocity ->
         case notePart of
            Event.NoteOn ->
               if velocity == Event.offVelocity
                 then (Event.NoteOff, Event.normalVelocity)
                 else (Event.NoteOn, velocity)
            _ -> (notePart, velocity)

processEvents ::
   (Storable a, Floating a, Monad m) =>
   PCM.Size ->
   PCM.SampleFreq ->
   [StampedEvent] ->
   MS.StateT (Time, Map.Map Pitch (OscillatorState a)) m [(Int, SV.Vector a)]
processEvents size rate input = do
   (chunkTime, oscis0) <- MS.get
   let pendingOscis =
          fmap
             (\(mplaying, finished) ->
                let mplayingNew =
                       fmap
                          (\(start,s0) ->
                             case renderTone (fromIntegral size - start) s0 of
                                (chunk, s1) -> ((start,chunk), s1))
                          mplaying
                in  (fmap snd mplayingNew,
                     maybe id (\p -> (fst p :)) mplayingNew $
                     map
                        (\(start, dur, s) -> (start, fst $ renderTone dur s))
                        finished)) $
          foldl
             (\oscis (time,ev) ->
                case Event.body ev of
                   Event.NoteEv noteEv note ->
                      case normalizeNote noteEv note of
                         (Event.NoteOn, velocity) ->
                            Map.insertWith
                               (\(newOsci, []) s ->
                                  {-
                                  A key may be pressed that was already pressed.
                                  This should not happen, but we must be prepared for it.
                                  Thus we call stopTone.
                                  -}
                                  (newOsci, stopTone time s))
                               (Event.noteNote note)
                               (Just (time,
                                   OscillatorState
                                      (0.2 * amplitudeFromVelocity velocity)
                                      (frequencyFromPitch (Event.noteNote note) /
                                       fromIntegral rate)
                                      0),
                                [])
                               oscis
                         (Event.NoteOff, _) ->
                            Map.adjust
                               (\s ->
                                  {-
                                  A key may be released that was not pressed.
                                  This should not happen, but we must be prepared for it.
                                  Thus stopTone also handles that case.
                                  -}
                                  (Nothing, stopTone time s))
                               (Event.noteNote note)
                               oscis
                         _ -> oscis
                   _ -> oscis)
             (fmap (\s -> (Just (0, s), [])) oscis0)
             (map (\(time,ev) -> (fromInteger (time-chunkTime), ev)) input)
   MS.put (chunkTime, Map.mapMaybe fst pendingOscis)
   return (concatMap snd $ Map.elems pendingOscis)


write ::
   (PCM.SampleFmt y) =>
   PCM.Handle PCM.Interleaved y -> SV.Vector y -> IO ()
write h xs =
   SVB.withStartPtr xs $ \buf ->
      fmap (const ()) . PCM.writeiRetry h buf . fromIntegral


{-
Caution:
 - MIDI clock and PCM clock are quite different:
   After running the synth for about an hour
   I got messages like "start too late: (8969,0)",
   that is, the MIDI clock was about 9000/44100 seconds
   ahead of the PCM clock.
-}
main :: IO ()
main =
   bracket openPCM closePCM $ \(size,rate,h) -> do
   putStrLn $ "period size: " ++ show size
   putStrLn $ "sample rate: " ++ show rate
   withInPort SndSeq.Block $ \sq port ->
      Queue.with sq $ \ q ->
      do setTimestamping sq port q
         Queue.control sq q Event.QueueStart Nothing
         _ <- Event.drainOutput sq
         write h (SV.replicate (2 * fromIntegral size) 0 :: SV.Vector Float)
         flip MS.evalStateT (0,Map.empty) $ forever $ do
            startTime <- MS.gets fst
            let stopTime = startTime + fromIntegral size
            evs <-
               liftIO $
               getStampedEventsUntilTime sq q port (fromIntegral rate) stopTime
            chunks <- processEvents size rate evs
            liftIO $
               write h (arrange size chunks :: SV.Vector Float)
            MS.modify $ \(_,ss) -> (stopTime, ss)
