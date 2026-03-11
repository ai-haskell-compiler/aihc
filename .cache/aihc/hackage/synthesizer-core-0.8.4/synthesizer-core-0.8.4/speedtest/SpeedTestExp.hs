module Main (main) where

import System.Time (getClockTime, diffClockTimes, tdSec, tdPicosec)

import qualified Data.StorableVector as V
import qualified Data.StorableVector.Base as VB
import Foreign.ForeignPtr (withForeignPtr)

import qualified Data.ByteString.Lazy as B
import qualified Data.Binary.Put as Bin

import Data.Array.IO (IOUArray, newArray_, hPutArray, writeArray)
import Data.Array.Unsafe (castIOUArray)

import Data.Word(Word8)

import System.IO (withBinaryFile, hPutBuf, IOMode(WriteMode))
import Foreign (Int16, pokeElemOff, allocaBytes)
import Control.Monad (zipWithM_)

import GHC.Float (double2Int)



{- INLINE exponential2  - makes things even worse -}

{- INLINE writeSignal -}

signalToBinaryPut :: [Int16] -> B.ByteString
signalToBinaryPut =
   Bin.runPut . mapM_ (Bin.putWord16host . fromIntegral)

writeSignalBinaryPut ::
   FilePath -> [Int16] -> IO ()
writeSignalBinaryPut fileName =
   B.writeFile fileName . signalToBinaryPut


round' :: Double -> Int16
round' x =
   fromIntegral (double2Int
     (if x<0 then x-0.5 else x+0.5))

_doubleToInt16 :: Double -> Int16
_doubleToInt16 x = round (32767 * x)

doubleToInt16' :: Double -> Int16
doubleToInt16' x = round' (32767 * x)

_doubleToInt16' :: Double -> Int16
_doubleToInt16' x = seq x 0


exponential2 :: Double -> Double -> [Double]
exponential2 hl y0 =
   let k = 0.5 ** recip hl
   in  iterate (k*) y0


writeSignal :: FilePath -> Int -> [Double] -> IO ()
writeSignal name num signal =
   withBinaryFile name WriteMode $ \h ->
   allocaBytes (2*num) $ \buf ->
      zipWithM_
         (pokeElemOff buf) [0..(num-1)]
         (map doubleToInt16' signal) >>
      hPutBuf h buf (2*num)

writeExponentialList :: FilePath -> Int -> Double -> Double -> IO ()
writeExponentialList name num hl y0 =
   withBinaryFile name WriteMode $ \h ->
   allocaBytes (2*num) $ \buf ->
      zipWithM_
         (pokeElemOff buf) [0..(num-1)]
         (map doubleToInt16' (let k = 0.5 ** recip hl
                              in  iterate (k*) y0)) >>
      hPutBuf h buf (2*num)

writeExponential :: FilePath -> Int -> Double -> Double -> IO ()
writeExponential name num hl y0 =
   withBinaryFile name WriteMode $ \h ->
   allocaBytes (2*num) $ \buf ->
{-
      let k = 0.5**(1/hl)
          loop :: Int -> Int -> IO ()
          loop i y =
             if i<num
               then pokeElemOff buf i (fromIntegral y :: Int16) >>
                    loop (succ i) (y+1)
               else return ()
      in  loop 0 (-10) >>
          hPutBuf h buf (2*num)
-}
      let k = 0.5**(1/hl)
          loop i y =
             if i<num
               then pokeElemOff buf i (doubleToInt16' y) >>
                    loop (succ i) (y*k)
               else return ()
      in  loop 0 y0 >>
          hPutBuf h buf (2*num)

writeExponentialIOUArray :: FilePath -> Int -> Double -> Double -> IO ()
writeExponentialIOUArray name num hl y0 =
   withBinaryFile name WriteMode $ \h ->
   newArray_ (0,2*num-1) >>= \arr ->
      let k = 0.5**(1/hl)
          loop :: Int -> Double -> IO ()
          loop i y =
             if i<num
               then writeArray (arr :: IOUArray Int Int16)
                       i (doubleToInt16' y) >>
                    loop (succ i) (y*k)
               else return ()
      in  loop 0 y0 >>
          castIOUArray arr >>= \word8arr ->
          hPutArray h (word8arr :: IOUArray Int Word8) (2*num)

writeExponentialStorableVector :: FilePath -> Int -> Double -> Double -> IO ()
writeExponentialStorableVector name num hl y0 =
   withBinaryFile name WriteMode $ \h ->
      let k = 0.5**(1/hl)
          (fp, _offset, _size) =
             VB.toForeignPtr $ fst $
             V.unfoldrN num (\y -> Just (doubleToInt16' y, y*k)) y0
      in  withForeignPtr fp $ \ buf -> hPutBuf h buf (2*num)



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

halfLife :: Double
halfLife = 100000


main :: IO ()
main =
   do measureTime "poke exponential int16"
         (writeExponential "exp-poked.sw" numSamples halfLife 1)
      measureTime "IOUArray exponential int16"
         (writeExponentialIOUArray "exp-iouarray.sw" numSamples halfLife 1)
      measureTime "StorableVector exponential int16"
         (writeExponentialStorableVector "exp-storablevector.sw" numSamples halfLife 1)
      measureTime "put exponential int16"
         (writeSignalBinaryPut "exp-int16string.sw"
            (take numSamples (map doubleToInt16' (exponential2 halfLife 1))))
      measureTime "poke exponential list of int16"
         (writeSignal "exp-list-poked.sw" numSamples (exponential2 halfLife 1))
      measureTime "poke exponential internal list of int16"
         (writeExponentialList "exp-intern-poked.sw" numSamples halfLife 1)
