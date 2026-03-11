{- |
Handling of warnings.
-}
module Sound.MIDI.Parser.Warning where

import qualified Sound.MIDI.Parser.Report as Report

import qualified Control.Monad.Exception.Synchronous as Sync
import qualified Control.Monad.Trans.Writer as Writer


type T m = Writer.WriterT [Report.UserMessage] m


run :: Monad m =>
   T m (Sync.Exceptional Report.UserMessage a) -> m (Report.T a)
run act =
   do (exc,warns) <- Writer.runWriterT act
      return $ Report.Cons warns (Sync.toEither exc)

{-
run :: Monad m =>
   T m a -> m (a, [Report.UserMessage])
run = Writer.runWriterT
-}

warn :: Monad m => String -> T m ()
warn text = Writer.tell [text]
