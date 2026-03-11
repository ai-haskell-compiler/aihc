module Reactive.Banana.JACK.Common where

import qualified Sound.JACK.MIDI as JackMidi
import qualified Sound.JACK.Exception as JackExc
import qualified Sound.JACK as Jack

import qualified Control.Monad.Exception.Synchronous as Sync

import Control.Monad.Trans.Reader (ReaderT, runReaderT, )


data Handle =
   Handle {
      client :: Jack.Client,
      portIn :: JackMidi.Port Jack.Input,
      portOut :: JackMidi.Port Jack.Output
   }

with :: ReaderT Handle (Sync.ExceptionalT JackExc.All IO) () -> IO ()
with f =
   Jack.handleExceptions $
      Jack.withClientDefault "Haskell-MIDI-Filter" $ \c ->
      Jack.withPort c "input" $ \input ->
      Jack.withPort c "output" $ \output ->
      runReaderT f $ Handle c input output
