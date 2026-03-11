module Common where

import qualified Sound.JACK as Jack
import qualified Sound.JACK.Exception as JackExc

import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Exception.Synchronous as Sync


mainWait ::
    JackExc.ThrowsErrno e =>
    Jack.Client -> String -> Sync.ExceptionalT e IO ()
mainWait client name =
    Jack.withActivation client $ Trans.lift $ do
        putStrLn $ "started " ++ name ++ "..."
        Jack.waitForBreak
