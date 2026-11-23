module Demo (watch) where

import GHC.Event (EventManager, IOCallback, Lifetime (..), evtRead, keyFd, registerFd)
import System.Posix.Types (Fd)

watch :: EventManager -> IOCallback -> Fd -> IO Fd
watch manager callback fd = keyFd <$> registerFd manager callback fd evtRead OneShot
