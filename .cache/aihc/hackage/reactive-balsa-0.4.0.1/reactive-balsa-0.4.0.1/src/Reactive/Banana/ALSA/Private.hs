{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Reactive.Banana.ALSA.Private where

import qualified Reactive.Banana.MIDI.Process as Process
import qualified Reactive.Banana.MIDI.Time as Time

import qualified Reactive.Banana.Bunch.Combinators as RB
import qualified Reactive.Banana.Bunch.Frameworks as RBF

import qualified Sound.ALSA.Sequencer as SndSeq
import qualified Sound.ALSA.Sequencer.Client as Client
import qualified Sound.ALSA.Sequencer.Port as Port
import qualified Sound.ALSA.Sequencer.Queue as Queue
import qualified Sound.ALSA.Sequencer.Event as Event

import qualified Control.Monad.Trans.Class as MT
import qualified Control.Monad.Trans.State as MS
import qualified Control.Monad.Trans.Reader as MR
import Control.Monad.IO.Class (MonadIO, )
import Control.Monad.Fix (MonadFix, )
import Control.Applicative (Applicative, )



data Handle =
   Handle {
      sequ :: SndSeq.T SndSeq.DuplexMode,
      client :: Client.T,
      portPublic, portPrivate :: Port.T,
      queue :: Queue.T
   }


newtype Reactor a =
   Reactor {
      runReactor ::
         MR.ReaderT
            (RBF.AddHandler Event.T, Handle)
            (MS.StateT Schedule RBF.MomentIO)
            a
   } deriving (Functor, Applicative, Monad, MonadIO, MonadFix)


instance RB.MonadMoment Reactor where
   liftMoment = Process.liftMomentIO . RB.liftMoment

instance Process.MomentIO Reactor where
   liftMomentIO = Reactor . MT.lift . MT.lift

instance Time.Timed Reactor where
   ticksFromSeconds =
      return .
      Time.cons . Time.Ticks .
      round . (nano *) .
      Time.unSeconds . Time.decons


nano :: Num a => a
nano = 1000^(3::Int)


{-
We need this to identify received Echo events.
We could also use the Custom fields of the Echo event
and would get a much larger range of Schedules,
but unfortunately we cannot use the Custom values
for selectively removing events from the output queue.
This is needed in our variable speed beat generator.

In order to prevent shortage of Tags
we could reserve one tag for events that will never be canceled
and then use the Custom fields in order to further distinguish Echo messages.
-}
type Schedule = Event.Tag
{-
newtype Schedule = Schedule Word32
   deriving (Eq, Ord, Enum, Show)
-}
