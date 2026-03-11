{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
module Main (main) where

--------------------------------------------------------------------------------

import Control.Exception (try, AsyncException(..))
import Control.Monad (forM_, when, unless)
import Data.Unique (hashUnique, newUnique)
import Data.Word (Word8, Word16)
import Foreign.Marshal.Array (peekArray, pokeArray, allocaArray)
import Foreign.Ptr (Ptr, nullPtr)
import System.Environment (getArgs)
import System.IO (hSetBuffering, stdin, BufferMode(..))

import Reliable.IO (Endpoint)
import qualified Reliable.IO as Reliable

--------------------------------------------------------------------------------

gMaxPacketBytes :: Num a => a
gMaxPacketBytes = 16 * 1024

randomInt :: Int -> Int -> IO Int
randomInt low high = do
  randInt <- hashUnique <$> newUnique
  return $ low + (randInt `mod` (high - low + 1))

seqNoToPktSz :: Word16 -> Int
seqNoToPktSz seqNo =
  ((fromIntegral seqNo * 1023) `mod` (gMaxPacketBytes - 2)) + 2

initializeSoak :: Double -> IO (Endpoint, Endpoint)
initializeSoak startTime = do
    putStrLn "initializing"
    Reliable.initialize
    Reliable.logLevel Reliable.LogLevel'Debug

    let clientConfig =
          Reliable.setPacketFragmentationLimit 500 $
          Reliable.setName "client" $
          Reliable.defaultConfig

        serverConfig =
          Reliable.setPacketFragmentationLimit 500 $
          Reliable.setName "server" $
          Reliable.defaultConfig

        recvFn :: Word16 -> Ptr Word8 -> Int -> IO Bool
        recvFn _ ptr sz = do
          when (ptr == nullPtr) $ fail "Received null pointer"
          when (sz <= 0) $ fail "Received empty packet"
          when (sz <= 2) $ fail "Received empty header"
          when (sz > gMaxPacketBytes) $ fail "Packet too large."
          
          (s0 : s1 : arr) <- peekArray sz ptr
          let seqNo :: Word16
              seqNo = (fromIntegral s0) + (256 * fromIntegral s1)

          unless (sz == seqNoToPktSz seqNo) $
            fail "Sequence number doesn't match packet size"

          forM_ (zip [(2 :: Int)..] arr) $ \(i, dat) ->
            unless (fromIntegral dat == (i + fromIntegral seqNo) `mod` 256) $
            fail "Malformed packet"

          return True

    rec clientEndpoint <- Reliable.createEndpoint clientConfig startTime
                            (xmitFn True) recvFn
        serverEndpoint <- Reliable.createEndpoint serverConfig startTime
                            (xmitFn False) recvFn

        let xmitFn :: Bool -> Word16 -> Ptr Word8 -> Int -> IO ()
            xmitFn toServer _ ptr sz = do
              rand <- randomInt 0 100
              let ep = if toServer then serverEndpoint else clientEndpoint
              unless (rand < 5) $ Reliable.receivePacket ep ptr sz

    return (clientEndpoint, serverEndpoint)

shutdownSoak :: Endpoint -> Endpoint -> IO ()
shutdownSoak client server = do
    putStrLn "shutdown"

    Reliable.destroyEndpoint client
    Reliable.destroyEndpoint server
    Reliable.terminate

iterateSoak :: Endpoint -> Endpoint -> Double -> IO ()
iterateSoak client server t =
  let generatePacketData :: Word16 -> [Word8]
      generatePacketData seqNo =
        (fromIntegral $ seqNo `mod` 256) :
        (fromIntegral $ (seqNo `div` 256) `mod` 256) :
        map (fromIntegral . (+ fromIntegral seqNo))
            [2..(seqNoToPktSz seqNo - 1)]

      sendNextPacket ep ptr = do
        seqNo <- Reliable.nextPacketSequence ep
        let pkt = generatePacketData seqNo
        pokeArray ptr pkt
        Reliable.sendPacket ep ptr (length pkt)

   in allocaArray gMaxPacketBytes $ \clientPktPtr ->
      allocaArray gMaxPacketBytes $ \serverPktPtr -> do
        sendNextPacket client clientPktPtr
        sendNextPacket server serverPktPtr

        Reliable.update client t
        Reliable.update server t

        Reliable.clearAcks client
        Reliable.clearAcks server
        
main :: IO ()
main = do
    hSetBuffering stdin LineBuffering
    args <- getArgs
    let startTime :: Double
        startTime = 100.0

        numIters :: Maybe Int
        numIters = case length args of
            1 -> Just $ read (head args)
            _ -> Nothing

        times = case numIters of
            Just x -> take x $ map (+startTime) [0.0,0.1..]
            Nothing -> map (+startTime) [0.0,0.1..]

    putStrLn "[soak]"
    putStrLn $ "num_iterations = " <> show numIters

    (client, server) <- initializeSoak startTime
    soakResult <- try $ forM_ times $ iterateSoak client server

    case soakResult of
        (Left UserInterrupt) -> return ()
        (Left e) -> fail (show e)
        (Right ()) -> return ()

    shutdownSoak client server
