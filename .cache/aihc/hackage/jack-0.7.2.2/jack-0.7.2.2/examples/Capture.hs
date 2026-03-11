{- |
This program creates a number of input ports
according to a list of given port names.
It captures the audio data from these ports
and writes the raw audio data in an interleaved way
to the file @captured.f32@.
You may convert it to an audio format with header information using @sox@.

Example:
> $ capture left right
> $ sox -c 2 -r 44100 captured.f32 captured.wav
-}
module Main where

import Common (mainWait)

import qualified Sound.JACK as Jack
import qualified Sound.JACK.Audio as JA

import qualified Sound.JACK.Exception as JackExc
import Sound.JACK (Process, Client, Port)

import qualified Control.Monad.Exception.Synchronous as Sync
import qualified Control.Monad.Trans.Cont as MC
import qualified Control.Monad.Trans.Class as Trans
import Data.Foldable (forM_, )

import Foreign.Storable (sizeOf, peek, )
import Foreign.Ptr (nullPtr, )
import Foreign.C.Error (eOK, )

import qualified System.Environment as Env
import qualified System.Console.GetOpt as GetOpt

import qualified System.IO as IO
import Data.Array.Base (getNumElements, )
import Data.Array.Storable
           (newArray_, readArray, writeArray, withStorableArray, )


main :: IO ()
main = do
    name <- Env.getProgName
    args <- Env.getArgs
    case args of
        [] ->
            putStrLn $
            GetOpt.usageInfo ("Usage: " ++ name ++ " JACK-PORT-NAME...") []
        _ -> capture name args

capture :: String -> [String] -> IO ()
capture name portNames =
    IO.withBinaryFile "captured.f32" IO.WriteMode $ \st ->
        Jack.handleExceptions $ flip MC.runContT return $ do
            client <- MC.ContT $ Jack.withClientDefault name
            inputs <- mapM (MC.ContT . Jack.withPort client) portNames
            Trans.lift $ setProcess st client inputs
            Trans.lift $ mainWait client name

setProcess ::
    (JackExc.ThrowsErrno e) =>
    IO.Handle ->
    Client ->
    [Port JA.Sample Jack.Input] ->
    Sync.ExceptionalT e IO ()
setProcess st client input =
    flip (Jack.setProcess client) nullPtr =<<
    (Trans.lift $ Jack.makeProcess $
     wrapFun st input)

wrapFun ::
    IO.Handle ->
    [Port JA.Sample Jack.Input] ->
    Process a
wrapFun st inputs nframes _args = do
    inArrs <- mapM (flip JA.getBufferArray nframes) inputs
    let (a,b) = Jack.nframesBounds nframes
        numChannels = length inputs
    output <- newArray_ ((a,0), (b, numChannels - 1))
    forM_ (Jack.nframesIndices nframes) $ \i ->
        forM_ (zip inArrs [0..]) $ \(inArr,c) ->
            writeArray output (i,c) =<< readArray inArr i
    withStorableArray output $ \p -> do
        n <- getNumElements output
        dummy <- peek p
        IO.hPutBuf st p $ sizeOf dummy * n
    return eOK
