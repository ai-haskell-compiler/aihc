module Reactive.Banana.ALSA.Example where

import qualified Reactive.Banana.ALSA.Sequencer as Seq
import qualified Reactive.Banana.ALSA.Common as Common
import qualified Reactive.Banana.ALSA.Time as AlsaTime

import qualified Reactive.Banana.MIDI.Training as Training
import qualified Reactive.Banana.MIDI.Pattern as Pattern
import qualified Reactive.Banana.MIDI.Controller as Ctrl
import qualified Reactive.Banana.MIDI.Pitch as Pitch
import qualified Reactive.Banana.MIDI.KeySet as KeySet
import qualified Reactive.Banana.MIDI.Process as Process
import qualified Reactive.Banana.MIDI.Note as Note
import qualified Reactive.Banana.MIDI.Time as Time
import Reactive.Banana.MIDI.Common
          (PitchChannel,
           program, channel, pitch, controller,
           singletonBundle, now, )

import qualified Reactive.Banana.MIDI.Utility as RBU

import qualified Reactive.Banana.Bunch.Combinators as RB
import Reactive.Banana.Bunch.Combinators ((<@>), )

import qualified Sound.MIDI.ALSA.Check as Check
import qualified Sound.MIDI.ALSA.Query as Query ()
import qualified Sound.MIDI.ALSA.Construct as Construct ()
import qualified Sound.ALSA.Sequencer.Event as Event
import Sound.MIDI.Message.Channel.Voice (Velocity, )

import qualified System.Random as Random

import Control.Monad.Trans.Reader (ReaderT, )
import Control.Monad (guard, liftM2, liftM3, join, )
import Control.Applicative (pure, (<*>), (<$>), )
import Data.Tuple.HT (mapFst, )
import Data.Maybe (mapMaybe, )

import Prelude hiding (reverse, )


run, runLLVM, runTimidity :: ReaderT Seq.Handle IO a -> IO a
run         x = Common.with $ Common.connectAny      >> x
runLLVM     x = Common.with $ Common.connectLLVM     >> x
runTimidity x = Common.with $ Common.connectTimidity >> x


pass,
   transpose,
   reverse,
   latch,
   groupLatch,
   delay,
   delayAdd,
   delayTranspose,
   cycleUp,
   cycleUpAuto,
   pingPong,
   pingPongAuto,
   binary,
   crossSum,
   bruijn,
   random,
   randomInversions,
   serialCycleUp,
   split,
   splitPattern,
   cyclePrograms,
   sweep,
   guitar,
   snapSelect,
   continuousSelect :: ReaderT Seq.Handle IO ()


time :: Rational -> AlsaTime.RelativeSeconds
time = Time.relative "example" . Time.Seconds

ticks :: Rational -> Seq.Reactor AlsaTime.RelativeTicks
ticks = Time.ticksFromSeconds . time

{-
stranspose ::
   (Query.C msg, Construct.C msg) => Int -> msg -> Maybe msg
-}
stranspose :: Int -> Event.Data -> Maybe Event.Data
stranspose d = Note.liftMaybe $ Note.transpose d

pass = Seq.run id
transpose = Seq.run $ RBU.mapMaybe $ stranspose 2
reverse = Seq.run $ RBU.mapMaybe $ Note.liftMaybe Note.reverse
-- works, but does not interact nicely with Note.AllOff
-- latch = Seq.run (Seq.bypass Common.maybeNote (fst . Seq.latch))
latch = Seq.runM (Seq.bypassM Note.maybeBndExt (fmap fst . Process.pressed KeySet.latch))
groupLatch = Seq.runM (Seq.bypassM Note.maybeBndExt (fmap fst . Process.pressed KeySet.groupLatch))
delay = Seq.runM $ \evs -> do dt <- ticks 0.2; return $ Process.delay dt evs
delayAdd = Seq.runM $ \evs -> do dt <- ticks 0.2; return $ Process.delayAdd dt evs
delayTranspose = Seq.runM $ \evs -> do
   let proc p dt = do
          tk <- ticks dt
          return $ Process.delay tk $ RBU.mapMaybe (stranspose p) evs
   fmap (foldl RB.union (fmap now evs)) $ sequence $
      proc  4 0.2 :
      proc  7 0.4 :
      proc 12 0.6 :
      []

getTempo ::
   (Check.C ev) =>
   RB.Event ev ->
   Seq.Reactor (RB.Behavior AlsaTime.RelativeTicks, RB.Event ev)
getTempo ctrl =
   join $
   liftM3 (uncurry Process.tempoCtrl Ctrl.tempoDefault)
      (ticks 0.15) (liftM2 (,) (ticks 0.5) (ticks 0.05)) (return ctrl)
{-
   pure 0.2
-}

pattern ::
   (KeySet.C set) =>
   set PitchChannel Velocity ->
   (RB.Behavior (set PitchChannel Velocity) ->
    RB.Event AlsaTime.AbsoluteTicks ->
    Seq.Reactor (RB.Event [Note.Boundary PitchChannel Velocity])) ->
   ReaderT Seq.Handle IO ()
pattern set pat = Seq.runTimesM $ \ times evs0 -> do
   (tempo, evs1) <- getTempo evs0
   beat <- Process.beatVar times tempo
   Seq.bypassM Note.maybeBndExt
      (\notes -> do
         pressed <- Process.pressed set notes
         pat (snd pressed) beat) evs1


serialCycleUp
         = pattern (KeySet.serialLatch 4) (Pattern.cycleUp (pure 4))
cycleUp  = pattern KeySet.groupLatch (Pattern.cycleUp (pure 4))
pingPong = pattern KeySet.groupLatch (Pattern.pingPong (pure 4))
binary   = pattern KeySet.groupLatch Pattern.binaryLegato
crossSum = pattern KeySet.groupLatch (Pattern.crossSum (pure 4))
bruijn   = pattern KeySet.groupLatch (Pattern.bruijn 4 2)
random   = pattern KeySet.groupLatch Pattern.random
randomInversions
         = pattern KeySet.groupLatch Pattern.randomInversions

cycleUpAuto = pattern KeySet.groupLatch $
   \set -> Pattern.cycleUp (fmap KeySet.size set) set
pingPongAuto = pattern KeySet.groupLatch $
   \set -> Pattern.pingPong (fmap KeySet.size set) set

cycleUpOffset ::
   ReaderT Seq.Handle IO ()
cycleUpOffset = Seq.runTimesM $ \ times evs0 -> do
   (tempo, evs1) <- getTempo evs0
   let n = 4
       range = 3 * fromIntegral n
   offset <-
      fmap round <$>
      Process.controllerLinear (channel 0) (controller 17)
         (0::Float) (-range,range) evs1
   beat <- Process.beatVar times tempo
   Seq.bypassM Note.maybeBndExt
      (\notes -> do
         pressed <- Process.pressed KeySet.groupLatch notes
         ixs <- Pattern.cycleUpIndex (pure n) beat
         Pattern.mono Pattern.selectFromOctaveChord
            (snd pressed)
            (pure (\o i -> mod (i-o) n + o) <*> offset <@> ixs))
      evs1


continuousSelect = Seq.runM $ \evs -> do
   pressed <-
      Process.pressed KeySet.groupLatch $ RBU.mapMaybe Note.maybeBndExt evs
   Pattern.mono Pattern.selectFromOctaveChord (snd pressed) =<<
      Process.uniqueChanges . fmap round =<<
      Process.controllerLinear (channel 0) (controller 17) (0::Float) (-8,16) evs

snapSelect = Seq.runM $ \evs -> do
   pressed <-
      Process.pressed KeySet.groupLatch $ RBU.mapMaybe Note.maybeBndExt evs
   ctrl <- Process.controllerRaw (channel 0) (controller 17) 64 evs
   Process.snapSelect (snd pressed) ctrl
{-
   let ctrl = Process.controllerRaw (channel 0) (controller 17) 64 evs
   Seq.bypass Note.maybeBndExt
      (\notes ->
         Seq.snapSelect (snd $ Process.pressed KeySet.groupLatch notes) ctrl) evs
-}

split = Seq.run $
   uncurry RB.union
   .
   mapFst
      (RBU.mapMaybe (stranspose 12)
       .
       fmap (Common.setChannel (channel 1)))
   .
   RBU.partition
      (\e ->
         (Common.checkChannel (channel 0 ==) e &&
          Common.checkPitch   (pitch 60 >) e) ||
         Common.checkController (controller 94 ==) e ||
         Common.checkController (controller 95 ==) e)


splitPattern = Seq.runTimesM $ \ times evs0 -> do
   (tempo, evs1) <- getTempo evs0
   beat <- Process.beatVar times tempo

   let checkLeft e = do
          bnd <- Note.maybeBndExt e
          case bnd of
             Note.BoundaryExt (Note.Boundary pc _vel _on) -> do
                guard (Pitch.extract pc < pitch 60)
                return bnd
             _ -> return bnd

   Seq.bypassM checkLeft
      (\left -> do
         pressed <- Process.pressed KeySet.groupLatch left
         fmap (mapMaybe (stranspose 12 . Note.fromBnd)) <$>
            Pattern.cycleUp (pure 4) (snd pressed) beat)
      evs1
{-
           RBU.mapMaybe (stranspose 12) left)) beat
-}


cyclePrograms = Seq.runTimesM $ \times evs -> do
--   Seq.cyclePrograms (map program [13..17]) times evs
   defer <- Time.ticksFromSeconds $ time 0.1
   pgms <- Process.cycleProgramsDefer defer (map program [13..17]) times evs
   return $ RB.union (RB.filterJust pgms) evs

sweep =
   Seq.runM $ \evs ->
      let c = channel 0
          centerCC = controller 70
          depthCC = controller 17
          speedCC = controller 16
      in do
          depth <- Process.controllerRaw c depthCC 64 evs
          center <- Process.controllerRaw c centerCC 64 evs
          speed <- Process.controllerExponential c speedCC 0.3 (0.1, 1) evs
          RB.union
               (RB.filterE
                  (not . Common.checkController
                     (flip elem [centerCC, depthCC, speedCC])) evs) .
            uncurry (Process.makeControllerLinear c centerCC depth center)
            <$>
            Process.sweep (time 0.01) (sin . (2*pi*)) speed

guitar =
   Seq.runM $ \evs ->
      (\f -> flip f evs =<< ticks 0.03) $ \del ->
      Seq.bypassM Note.maybeBndExt $ \notes ->
      let (trigger, keys) =
             RBU.partitionMaybe
                (\note ->
                   case note of
                      Note.BoundaryExt (Note.Boundary pc _vel on) -> do
                         guard $ Pitch.extract pc == pitch 84
                         return on
                      _ -> Nothing)
                notes
      in  do
            pressed <- snd <$> Process.pressed KeySet.groupLatch keys
            Process.guitar del pressed trigger
               :: Seq.Reactor (RB.Event Common.EventDataBundle)


trainer ::
   (Random.RandomGen g) =>
   g -> ReaderT Seq.Handle IO ()
trainer g =
   Seq.runTimesM $ \ times evs ->
      RB.union (fmap singletonBundle evs) <$>
      Process.trainer (channel 0) (time 0.5) (time 0.3) (Training.all g) times evs
