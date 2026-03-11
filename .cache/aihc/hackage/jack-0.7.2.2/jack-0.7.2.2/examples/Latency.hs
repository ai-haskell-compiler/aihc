-- This example demonstrates the use of JACK latency API
module Main where

import Foreign.C.Types (CFloat)
import qualified Sound.JACK.Audio as Audio
import qualified Sound.JACK as JACK
import System.Environment (getProgName, )
import Data.IORef
import Common

main :: IO ()
main = do
    name <- getProgName
    ref <- newIORef 0
    JACK.handleExceptions $
        JACK.withClientDefault name $ \client ->
        JACK.withPort client "input" $ \input ->
        JACK.withPort client "output" $ \output ->
        JACK.withLatency client (latency input output) $
        Audio.withProcessMono client input (process ref) output $
            mainWait client name

process :: IORef CFloat -> CFloat -> IO CFloat
process r v = do
    v' <- readIORef r
    writeIORef r v
    return v'

latency ::
    Audio.Port JACK.Input  ->
    Audio.Port JACK.Output ->
    JACK.LatencyCallbackMode ->
    IO ()
latency input output mode =
    if mode == JACK.jackCaptureLatency
        then modifyLatencyRange "Capture" input output mode
        else modifyLatencyRange "Playback" output input mode

modifyLatencyRange ::
    String ->
    Audio.Port from ->
    Audio.Port to ->
    JACK.LatencyCallbackMode ->
    IO ()
modifyLatencyRange name from to mode = do
    JACK.LatencyRange (JACK.NFrames a) (JACK.NFrames b) <-
        JACK.getLatencyRange from mode
    putStrLn $ name ++ " latency recalculation: " ++ show a ++ " " ++ show b
    JACK.setLatencyRange to mode $
        JACK.LatencyRange (JACK.NFrames $ a+1) (JACK.NFrames $ b+1)
