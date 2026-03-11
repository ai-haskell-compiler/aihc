{- |
Handling of exceptions.
-}
module Sound.MIDI.Parser.Exception where

import qualified Sound.MIDI.Parser.Report as Report

import qualified Control.Monad.Exception.Synchronous as Sync


type T m = Sync.ExceptionalT Report.UserMessage m


run :: Monad m =>
   T m a -> m (Sync.Exceptional Report.UserMessage a)
run = Sync.runExceptionalT


giveUp :: Monad m => String -> T m a
giveUp = Sync.throwT

try :: Monad m => T m a -> m (Sync.Exceptional Report.UserMessage a)
try = Sync.tryT
