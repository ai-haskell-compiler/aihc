-- This example demonstrates the use of various specialized JACK callbacks.
module Main where

import Foreign.C.Types (CFloat)
import qualified Control.Monad.Exception.Synchronous as Sync
import qualified Control.Monad.Trans.Class as Trans
import qualified Foreign.C.Error as E
import qualified Sound.JACK.Audio as Audio
import qualified Sound.JACK as JACK
import System.Environment (getProgName, )
import Common

main :: IO ()
main = do
    name <- getProgName
    JACK.handleExceptions $
        JACK.withClientDefault name $ \client ->
        JACK.withPort client "input" $ \input ->
        JACK.withPort client "output" $ \output ->
        JACK.withFreewheel client freewheel $
        JACK.withBufferSize client bufferSize $
        JACK.withSampleRate client sampleRate $
        JACK.withPortConnect client portConnect $
        JACK.withPortRename client portRename $
        JACK.withPortRegistration client portRegistration $
        JACK.withClientRegistration client clientRegistration $
        JACK.withGraphOrder client graphOrder $
        JACK.withXRun client xRun $
        Audio.withProcessMono client input process output $
            mainWait client name

process :: CFloat -> IO CFloat
process s = return s

-- Signals entering freewheel mode
freewheel :: Bool -> IO ()
freewheel b = putStrLn $ "Freewheel: " ++ show b

-- Signals the change of buffer size
bufferSize :: JACK.NFrames -> Sync.ExceptionalT E.Errno IO ()
bufferSize s = Trans.lift $ putStrLn $ "Buffer size: " ++ show s

-- Signals the change of sample rate
sampleRate :: JACK.NFrames -> Sync.ExceptionalT E.Errno IO ()
sampleRate s = Trans.lift $ putStrLn $ "Sample rate: " ++ show s

-- Signals port connection
portConnect :: JACK.PortId -> JACK.PortId -> Bool -> IO ()
portConnect p1 p2 True  = putStrLn $ "Port connect: " ++ show p1 ++ " to " ++ show p2
portConnect p1 p2 False = putStrLn $ "Port disconnect: " ++ show p1 ++ " to " ++ show p2

-- Signals the change of port name
portRename :: JACK.PortId -> String -> String -> IO ()
portRename p s1 s2 = putStrLn $ "Port rename: " ++ show p ++ ", " ++ s1 ++ " to " ++ s2

-- Signals port registration
portRegistration :: JACK.PortId -> Bool -> IO ()
portRegistration p True  = putStrLn $ "Port registered: " ++ show p
portRegistration p False = putStrLn $ "Port unregistered: " ++ show p

-- Signals client registration
clientRegistration :: String -> Bool -> IO ()
clientRegistration s True  = putStrLn $ "Client registered: " ++ s
clientRegistration s False = putStrLn $ "Client unregistered: " ++ s

-- Signals reordering of the JACK graph
graphOrder :: Sync.ExceptionalT E.Errno IO ()
graphOrder = Trans.lift $ putStrLn "Processing graph reordered"

-- Signals Xruns
xRun :: Sync.ExceptionalT E.Errno IO ()
xRun = Trans.lift $ putStrLn "Xrun happened"

