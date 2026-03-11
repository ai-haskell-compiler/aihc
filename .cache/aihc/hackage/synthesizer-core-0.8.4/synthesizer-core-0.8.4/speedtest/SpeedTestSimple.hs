module Main (main) where

import System.Time (getClockTime, diffClockTimes, tdSec, tdPicosec)

import qualified Data.ByteString.Lazy as B
import qualified Data.Binary.Put as Bin

import Foreign (Int16)


signalToBinaryPut :: [Int16] -> B.ByteString
signalToBinaryPut =
   Bin.runPut . mapM_ (Bin.putWord16host . fromIntegral)

writeSignalBinaryPut ::
   FilePath -> [Int16] -> IO ()
writeSignalBinaryPut fileName =
   B.writeFile fileName . signalToBinaryPut


measureTime :: String -> IO () -> IO ()
measureTime name act =
   do putStr (name++": ")
      timeA <- getClockTime
      act
      timeB <- getClockTime
      let td = diffClockTimes timeB timeA
      print (fromIntegral (tdSec td) +
             fromInteger (tdPicosec td) * 1e-12 :: Double)

numSamples :: Int
numSamples = 1000000

zeroSignal16 :: [Int16]
zeroSignal16 = replicate numSamples 0

zeroByteString :: B.ByteString
zeroByteString = B.replicate (fromIntegral (2 * numSamples)) 0

main :: IO ()
main =
   do measureTime "write zero bytestring"
         (B.writeFile "zero-bytestring.sw" zeroByteString)
      measureTime "put zero int16"
         (writeSignalBinaryPut "zero-int16string.sw" zeroSignal16)
