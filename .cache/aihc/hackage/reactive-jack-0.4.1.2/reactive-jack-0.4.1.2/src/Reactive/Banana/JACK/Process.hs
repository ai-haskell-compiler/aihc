{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Reactive.Banana.JACK.Process where

import qualified Reactive.Banana.JACK.Common as Common

import qualified Reactive.Banana.MIDI.Process as Process
import qualified Reactive.Banana.MIDI.Time as Time

import qualified Reactive.Banana.Bunch.Combinators as RB
import qualified Reactive.Banana.Bunch.Frameworks as RBF
import Reactive.Banana.Bunch.Frameworks (Handler, )
import Reactive.Banana.Bunch.Combinators ((<@>), )

import qualified Sound.JACK.Exception as JackExc
import qualified Sound.JACK.MIDI as JackMidi
import qualified Sound.JACK as Jack

import qualified Sound.MIDI.Message as MidiMsg

import qualified Data.EventList.Absolute.TimeBody as EventListAbs
import qualified Data.Map as Map
import Data.Map (Map)

import Numeric.NonNegative.Class ((-|), )

import qualified Control.Monad.Exception.Synchronous as Sync
import qualified Foreign.C.Error as E

import qualified Data.Accessor.Basic as Acc

import qualified Control.Monad.Trans.Class as MT
import qualified Control.Monad.Trans.State as MS
import qualified Control.Monad.Trans.Reader as MR
import Control.Monad.Trans.Reader (ReaderT, runReaderT, )
import Control.Monad.IO.Class (MonadIO, liftIO, )
import Control.Monad.Fix (MonadFix, )
import Control.Monad (when, )
import Control.Applicative (Applicative, pure, (<*>), )
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef, )
import Data.Foldable (forM_, )
import Data.Monoid (Monoid, mappend, mempty, )
import Data.Semigroup (Semigroup, (<>), )

import Prelude hiding (sequence, )



-- * make JACK reactive

data Context =
   Context {
      contextCycleStartRef :: IORef AbsoluteTicks,
      contextInQueueRef :: IORef InQueue,
      contextClient :: Jack.Client
   }

newtype Schedule = Schedule Int
   deriving (Eq, Ord, Enum, Show)

newtype Reactor a =
   Reactor {
      runReactor :: ReaderT Context (MS.StateT Schedule RBF.MomentIO) a
   } deriving (Functor, Applicative, Monad, MonadIO, MonadFix)


instance RB.MonadMoment Reactor where
   liftMoment = Process.liftMomentIO . RB.liftMoment

instance Process.MomentIO Reactor where
   liftMomentIO = Reactor . MT.lift . MT.lift

instance Time.Timed Reactor where
   ticksFromSeconds t = do
      client <- Reactor $ MR.asks contextClient
      rate <- liftIO $ Jack.getSampleRate client
      return .
         Time.cons . Time.Ticks .
         round . (fromIntegral rate *) .
         Time.unSeconds . Time.decons $ t


data InEvent =
   InEvent (Map Schedule [Handler AbsoluteTicks]) [MidiMsg.T]

instance Semigroup InEvent where
   InEvent xs0 ys0 <> InEvent xs1 ys1 =
      InEvent (Map.unionWith (++) xs0 xs1) (ys0++ys1)

instance Monoid InEvent where
   mempty = InEvent Map.empty []
   mappend = (<>)

inEventBeat :: Acc.T InEvent (Map Schedule [Handler AbsoluteTicks])
inEventBeat =
   Acc.fromSetGet
      (\beats (InEvent _ evs) -> InEvent beats evs)
      (\(InEvent beats _) -> beats)


type InQueue = Map Jack.NFrames InEvent
type OutQueue = Map Jack.NFrames [MidiMsg.T]

run ::
   (JackExc.ThrowsErrno e) =>
   (RB.Event MidiMsg.T -> RB.Event [MidiMsg.T]) ->
   ReaderT Common.Handle (Sync.ExceptionalT e IO) ()
run f =
   runM (\ _ts xs -> return $ f xs)

runM ::
   (JackExc.ThrowsErrno e) =>
   (RB.Behavior AbsoluteTicks ->
    RB.Event MidiMsg.T -> Reactor (RB.Event [MidiMsg.T])) ->
   ReaderT Common.Handle (Sync.ExceptionalT e IO) ()
runM f = do
   MR.ReaderT $ \h -> do
      cycleStartRef <- MT.lift $ newIORef mempty
      inQueueRef    <- MT.lift $ newIORef Map.empty
      outQueueRef   <- MT.lift $ newIORef Map.empty
      (addEventHandler, runEventHandler) <- MT.lift RBF.newAddHandler
      (addTimeHandler,  runTimeHandler)  <- MT.lift RBF.newAddHandler
      MT.lift $ RBF.actuate =<< RBF.compile (do
         time <- RBF.fromChanges (error "uninitialized time") addTimeHandler
         evs <-
            flip MS.evalStateT (Schedule 0) .
            flip runReaderT
               (Context cycleStartRef inQueueRef (Common.client h)) .
            runReactor .
            f (fmap (absTicksFromNFrames . fst) time)
               =<< RBF.fromAddHandler addEventHandler
         RBF.reactimate $
            pure
               (\t -> modifyIORef outQueueRef . Map.insertWith (++) t)
               <*> fmap snd time <@> evs)
      Jack.withProcess (Common.client h)
            (process h cycleStartRef inQueueRef outQueueRef
               runTimeHandler runEventHandler) $
         Jack.withActivation (Common.client h) $
            MT.lift $ Jack.waitForBreak



viewQueueL ::
   InQueue ->
   Maybe
      ((Jack.NFrames, Either [Handler AbsoluteTicks] MidiMsg.T),
       InQueue)
viewQueueL q0 = do
   ((t, InEvent beats evs), q1) <- Map.minViewWithKey q0
   case (Map.minView beats, evs) of
      (Just (b,bs), _) ->
         Just ((t, Left b), Map.insert t (InEvent bs evs) q1)
      (Nothing, e:es) ->
         Just ((t, Right e),
               if null es then q1 else Map.insert t (InEvent Map.empty es) q1)
      (Nothing, []) -> viewQueueL q1

normalizeInQueue :: InQueue -> InQueue
normalizeInQueue =
   Map.filter (\(InEvent beats evs) -> not $ Map.null beats && null evs)

reduceQueueTime ::
   Jack.NFrames -> Map Jack.NFrames a -> Map Jack.NFrames a
reduceQueueTime dt = Map.mapKeysMonotonic (-| dt)


process ::
   Common.Handle ->
   IORef AbsoluteTicks ->
   IORef InQueue ->
   IORef OutQueue ->
   Handler (Jack.NFrames, Jack.NFrames) ->
   Handler MidiMsg.T ->
   Jack.NFrames ->
   Sync.ExceptionalT E.Errno IO ()

process h cycleStartRef inQueueRef outQueueRef
      runTimeHandler runEventHandler size = do

   evs <- JackMidi.readEventsFromPort (Common.portIn h) size
   MT.lift $ modifyIORef inQueueRef $
      Map.unionWith mappend $ fmap (InEvent Map.empty) $
      Map.fromListWith (++) $ EventListAbs.toPairList $
      fmap (:[]) evs

   {-
   ToDo:
   How to handle overflow properly?
   jack.h does not give an advice.
   -}
   cycleStart <- MT.lift $ Jack.lastFrameTime $ Common.client h
   let cycleStartTicks = absTicksFromNFrames cycleStart
   MT.lift $ writeIORef cycleStartRef cycleStartTicks
   -- MT.lift $ print cycleStart
   let duration = size
       loop = do
          inQueueStart <- readIORef inQueueRef
          forM_ (viewQueueL inQueueStart) $ \((t,ev), remQueue) ->
             when (t<duration) $ do
                writeIORef inQueueRef remQueue
                -- the handlers may modify the inQueue in inQueueRef
                runTimeHandler (mappend cycleStart t, t)
                -- the handlers may modify the outQueue in outQueueRef
                either
                   (mapM_ ($ Time.inc (ticksFromNFrames t) cycleStartTicks))
                   runEventHandler ev
                loop

   MT.lift loop

   MT.lift $ modifyIORef inQueueRef $ reduceQueueTime duration

   outQueue <- MT.lift $ readIORef outQueueRef
   case Map.splitLookup duration outQueue of
      (now, pivot, later) -> do
         JackMidi.writeEventsToPort (Common.portOut h) size $
            EventListAbs.flatten $ EventListAbs.fromPairList $ Map.toList now
         MT.lift $ writeIORef outQueueRef $
            maybe id (Map.insert mempty) pivot $
            reduceQueueTime duration later



type RelativeTicks = Time.T Reactor Time.Relative Time.Ticks
type AbsoluteTicks = Time.T Reactor Time.Absolute Time.Ticks
type RelativeSeconds = Time.T Reactor Time.Relative Time.Seconds

nframesFromTicks :: RelativeTicks -> Jack.NFrames
nframesFromTicks =
   Jack.NFrames . fromIntegral . Time.unTicks . Time.decons

ticksFromNFrames :: Jack.NFrames -> RelativeTicks
ticksFromNFrames (Jack.NFrames n) =
   Time.cons . Time.Ticks . fromIntegral $ n

absTicksFromNFrames :: Jack.NFrames -> AbsoluteTicks
absTicksFromNFrames (Jack.NFrames n) =
   Time.cons . Time.Ticks . fromIntegral $ n


sendBeat ::
   Context -> Schedule ->
   Handler AbsoluteTicks -> AbsoluteTicks -> IO ()
sendBeat context sched runBeatHandler time = do
   cycleStart <- readIORef (contextCycleStartRef context)
   modifyIORef (contextInQueueRef context) $
      Map.insertWith mappend
         (nframesFromTicks $ Time.subSat time cycleStart)
         (InEvent (Map.singleton sched [runBeatHandler]) [])

cancelBeats :: Context -> Schedule -> IO ()
cancelBeats context sched = do
   modifyIORef (contextInQueueRef context) $
      normalizeInQueue .
      fmap (Acc.modify inEventBeat (Map.delete sched))


instance Process.Reactor Reactor where
   reserveSchedule = Reactor $ MR.ReaderT $ \context -> do
      sched <- MS.get
      MS.modify succ
      (eEcho, runBeatHandler) <- MT.lift RBF.newEvent
      return
         (mapM_ (sendBeat context sched runBeatHandler),
          cancelBeats context sched, eEcho)
