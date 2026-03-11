module Main where

import Common (mainWait)

import qualified Sound.JACK.Audio as Audio
import qualified Sound.JACK as JACK

import qualified Control.Monad.Exception.Synchronous as Sync
import qualified Control.Monad.Trans.Class as Trans
import qualified Foreign.C.Error as E

import System.Environment (getProgName)

import Data.Array.Storable (writeArray, )


main :: IO ()
main = do
    name <- getProgName
    JACK.handleExceptions $
        JACK.withClientDefault name $ \client ->
        JACK.withPort client "output" $ \output ->
        JACK.withProcess client (process output) $
            mainWait client name

process ::
    Audio.Port JACK.Output -> JACK.NFrames -> Sync.ExceptionalT E.Errno IO ()
process output nframes = Trans.lift $ do
    outArr <- Audio.getBufferArray output nframes
    case JACK.nframesIndices nframes of
        [] -> return ()
        zero:rest -> do
            writeArray outArr zero 1
            mapM_ (\i -> writeArray outArr i 0) rest
