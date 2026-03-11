module Reactive.Banana.ALSA.Sequencer (
   Handle,
   Reactor,
   module Reactive.Banana.ALSA.Sequencer,
   ) where

import qualified Reactive.Banana.ALSA.Common as Common
import qualified Reactive.Banana.ALSA.Time as AlsaTime
import Reactive.Banana.ALSA.Private
          (Reactor(Reactor, runReactor), Schedule,
           Handle(sequ, client, portPrivate), )

import qualified Reactive.Banana.MIDI.Time as Time
import qualified Reactive.Banana.MIDI.Process as Process
import qualified Reactive.Banana.MIDI.Utility as RBU
import Reactive.Banana.MIDI.Common (Future(Future), )

import qualified Reactive.Banana.Bunch.Combinators as RB
import qualified Reactive.Banana.Bunch.Frameworks as RBF
import Reactive.Banana.Bunch.Combinators ((<@>), )

import qualified Sound.ALSA.Sequencer.Event.RemoveMonad as Remove
import qualified Sound.ALSA.Sequencer.Event as Event
import qualified Sound.ALSA.Sequencer.Address as Addr

import qualified Control.Monad.Trans.Class as MT
import qualified Control.Monad.Trans.State as MS
import qualified Control.Monad.Trans.Reader as MR
import Control.Monad.Trans.Reader (ReaderT(ReaderT), )
import Control.Monad (liftM, forever, )
import Control.Applicative ((<$>), )
import Control.Functor.HT (void, )
import Data.Monoid (mempty, )

import Prelude hiding (sequence, )



startSchedule :: Schedule
startSchedule = Event.Tag 1

nextSchedule :: Schedule -> Schedule
nextSchedule (Event.Tag s) =
   if s == maxBound
     then error $ "maximum number of schedules " ++ show s ++ " reached"
     else Event.Tag $ succ s


getHandle :: Reactor Handle
getHandle = Reactor $ MR.asks snd

run ::
   (Common.Events ev) =>
   (RB.Event Event.Data -> RB.Event ev) ->
   ReaderT Handle IO ()
run f = runM (return . f)

runM ::
   (Common.Events ev) =>
   (RB.Event Event.Data -> Reactor (RB.Event ev)) ->
   ReaderT Handle IO ()
runM f = runTimesM (const f)

runTimesM ::
   (Common.Events ev) =>
   (RB.Behavior AlsaTime.AbsoluteTicks ->
    RB.Event Event.Data -> Reactor (RB.Event ev)) ->
   ReaderT Handle IO ()
runTimesM f = do
   Common.startQueue
   MR.ReaderT $ \h -> do
      (addEventHandler, runEventHandler) <- RBF.newAddHandler
      (addEchoHandler,  runEchoHandler)  <- RBF.newAddHandler
      (addTimeHandler,  runTimeHandler)  <- RBF.newAddHandler
      RBF.actuate =<< RBF.compile (do
         time <- RBF.fromChanges mempty addTimeHandler
         evs <-
            flip MS.evalStateT startSchedule
              . flip MR.runReaderT (addEchoHandler, h)
              . runReactor
              . f time
              . fmap Event.body
            =<< RBF.fromAddHandler addEventHandler
         RBF.reactimate $
            outputEvents h <$> time <@> RB.collect evs)
      forever $ do
         ev <- Event.input (sequ h)
         runTimeHandler $ AlsaTime.fromEvent ev
         if Event.dest ev == Addr.Cons (client h) (portPrivate h)
           then debug "input: echo"  >> runEchoHandler ev
           else debug "input: event" >> runEventHandler ev

outputEvents ::
   Common.Events evs =>
   Handle -> AlsaTime.AbsoluteTicks -> evs -> IO ()
outputEvents h time evs = do
   mapM_ (Event.output (sequ h)) $
      map (\(Future dt body) ->
             Common.makeEvent h (Time.inc dt time) body) $
      Common.flattenEvents evs
   void $ Event.drainOutput (sequ h)


checkSchedule :: Schedule -> Event.T -> Bool
checkSchedule sched echo =
   maybe False (sched ==) $ do
      Event.CustomEv Event.Echo _ <- Just $ Event.body echo
      return $ Event.tag echo

reactimate :: RB.Event (IO ()) -> Reactor ()
reactimate evs =
   Process.liftMomentIO $ RBF.reactimate evs

sendEchos :: Handle -> Schedule -> [AlsaTime.AbsoluteTicks] -> IO ()
sendEchos h sched echos = do
   flip mapM_ echos $ \time ->
      Event.output (sequ h) $
      (Common.makeEcho h time)
      { Event.tag = sched }
   void $ Event.drainOutput (sequ h)
   debug "echos sent"

cancelEchos :: Handle -> Schedule -> IO ()
cancelEchos h sched =
   Remove.run (sequ h) $ do
      Remove.setOutput
      Remove.setEventType Event.Echo
      Remove.setTag sched

instance Process.Reactor Reactor where
   reserveSchedule = Reactor $ ReaderT $ \(addH,h) -> do
      sched <- MS.get
      MS.modify nextSchedule
      eEcho <-
         MT.lift $
         fmap (fmap AlsaTime.fromEvent .
               RB.filterE (checkSchedule sched)) $
         RBF.fromAddHandler addH
      return (sendEchos h sched, cancelEchos h sched, eEcho)


debug :: String -> IO ()
debug =
   const $ return ()
   -- putStrLn


bypass ::
   (Common.Events a, Common.Events c) =>
   (a -> Maybe b) ->
   (RB.Event b -> RB.Event c) ->
   RB.Event a -> RB.Event [Common.Future Event.Data]
bypass p f =
   RBU.bypass p (fmap Common.flattenEvents) (fmap Common.flattenEvents . f)

bypassM ::
   (Monad m, Common.Events a, Common.Events c) =>
   (a -> Maybe b) ->
   (RB.Event b -> m (RB.Event c)) ->
   RB.Event a -> m (RB.Event [Common.Future Event.Data])
bypassM p f =
   RBU.bypassM p
      (return . fmap Common.flattenEvents)
      (liftM (fmap Common.flattenEvents) . f)
