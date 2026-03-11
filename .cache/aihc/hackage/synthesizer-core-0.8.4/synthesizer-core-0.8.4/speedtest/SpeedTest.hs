{-# LANGUAGE NoImplicitPrelude #-}
module Main (main) where

import System.Time (getClockTime, diffClockTimes, tdSec, tdPicosec)
import System.Directory (removeFile)

-- the strict ByteString variant is not faster here
import qualified Data.ByteString.Lazy as B
import qualified Data.Binary.Put as Bin

import Foreign (Int16, Ptr, alloca, allocaBytes, poke, pokeElemOff, sizeOf)
import System.IO (withBinaryFile, IOMode(WriteMode), Handle, hPutBuf)

import qualified Algebra.Transcendental as Trans
import qualified Algebra.RealField      as RealField

import GHC.Float (double2Int)

import Data.Word (Word8)

import Control.Monad (when, foldM, zipWithM_, )
import Data.List (unfoldr)
import Data.Maybe.HT (toMaybe, )
import Data.List.HT (sliceVertical, )

import NumericPrelude.Base
import NumericPrelude.Numeric

import qualified Prelude as P98

{-
  ghc -prof -auto-all -O -fvia-C SpeedTest.hs
  a.out +RTS -p -RTS
-}


{-# SPECIALIZE osciModSaw :: Double -> [Double] -> [Double] #-}
{-# SPECIALIZE freqToPhase :: Double -> [Double] -> [Double] #-}
{-# SPECIALIZE exponential2 :: Double -> Double -> [Double] #-}
{-# SPECIALIZE clip :: Double -> Double -> Double -> Double #-}
{-# SPECIALIZE numToInt :: Double -> Int #-}
{-# SPECIALIZE numToInt16 :: Double -> Int16 #-}

{- INLINE zeroSignal #-}
{- INLINE sawSignal #-}
{- INLINE zeroSignal16 #-}
{- INLINE sawSignal16 #-}


{- |
saw tooth oscillator with modulated frequency
-}
osciModSaw :: RealField.C a => a -> [a] -> [a]
osciModSaw phase freq = map (\x -> 2*x-1) (freqToPhase phase freq)

{- |
Convert a list of phase steps into a list of momentum phases
phase is a number in the interval [0,1)
freq contains the phase steps
-}
freqToPhase :: RealField.C a => a -> [a] -> [a]
freqToPhase phase freq =
   scanl (\curphase dif -> fraction (curphase+dif)) phase freq

exponential2 :: Trans.C a => a -> a -> [a]
exponential2 halfLife y0 =
   let k = 0.5**(1/halfLife)
   in  iterate (k*) y0



-- write the signal as binary file containing 16 bit words
writeSignalMono, writeSignalMonoS ::
   FilePath -> [Int16] -> IO ()
writeSignalMono fileName signal =
   writeFile fileName (signalToBinaryMono signal)
writeSignalMonoS fileName signal =
   writeFile fileName (signalToBinaryMonoS signal)

signalToBinaryMono, signalToBinaryMonoS ::
   [Int16] -> String
signalToBinaryMono  = concatMap (int16ToChars . P98.fromIntegral)
signalToBinaryMonoS = foldr int16ToCharsS [] . map P98.fromIntegral

writeSignalMonoInt ::
   FilePath -> [Int] -> IO ()
writeSignalMonoInt fileName signal =
   writeFile fileName (signalToBinaryMonoInt signal)

signalToBinaryMonoInt :: [Int] -> String
signalToBinaryMonoInt = concatMap int16ToChars


writeSignalMonoBStr :: FilePath -> [Int16] -> IO ()
writeSignalMonoBStr fileName =
   B.writeFile fileName . signalToBinaryMonoBStr

signalToBinaryMonoBStr :: [Int16] -> B.ByteString
signalToBinaryMonoBStr =
   B.pack . concatMap (int16ToBytes . P98.fromIntegral)


writeSignalMonoBinaryPut ::
   FilePath -> [Int16] -> IO ()
writeSignalMonoBinaryPut fileName =
   B.writeFile fileName . signalToBinaryBinaryPut

signalToBinaryBinaryPut :: [Int16] -> B.ByteString
signalToBinaryBinaryPut =
   Bin.runPut . mapM_ (Bin.putWord16host . P98.fromIntegral)


writeSignalMonoBinaryIntPut ::
   FilePath -> [Int] -> IO ()
writeSignalMonoBinaryIntPut fileName =
   B.writeFile fileName . signalToBinaryBinaryIntPut

signalToBinaryBinaryIntPut :: [Int] -> B.ByteString
signalToBinaryBinaryIntPut =
   Bin.runPut . mapM_ (Bin.putWord16host . P98.fromIntegral)



-- from BinarySample
clip :: Ord a => a -> a -> a -> a
clip lower upper = max lower . min upper

numToInt :: (RealField.C a) => a -> Int
numToInt x = round (32767 * clip (-1) 1 x)

-- from BinarySample
-- return type could be Int16, but that is not well supported by NumericPrelude.Numeric
numToInt16 :: (RealField.C a) => a -> Int16
numToInt16 = P98.fromIntegral . numToInt

roundDouble :: Double -> Int
roundDouble x =
   double2Int (if x<0 then x-0.5 else x+0.5)

doubleToInt :: Double -> Int
doubleToInt x = roundDouble (32767 * clip (-1) 1 x)

_doubleToInt16 :: Double -> Int16
_doubleToInt16 = P98.fromIntegral . doubleToInt




int16ToChars :: Int -> String
int16ToChars x =
   let (hi,lo) = divMod x 256
   in  [toEnum lo, toEnum (mod hi 256)]

int16ToCharsS :: Int -> String -> String
int16ToCharsS x s =
   let (hi,lo) = divMod x 256
   in  toEnum lo : toEnum (mod hi 256) : s

int16ToBytes :: Int -> [Word8]
int16ToBytes x =
   let (hi,lo) = divMod x 256
--    in  [P98.fromIntegral lo, P98.fromIntegral (mod hi 256)]
   in  [P98.fromIntegral lo, P98.fromIntegral hi]
        -- conversion to Word8 wraps silently to positive values


{- * machine oriented techniques -}

writeSignalMonoPoke ::
   FilePath -> [Int16] -> IO ()
writeSignalMonoPoke fileName signal =
   withBinaryFile fileName WriteMode $
      \h -> alloca $
         \p -> mapM_ (putInt h p) signal

putInt :: Handle -> Ptr Int16 -> Int16 -> IO ()
putInt h p n =
   poke p n >> hPutBuf h p (sizeOf n)


maxBlockSize :: Int
maxBlockSize = 1000

int16size :: Int
int16size = sizeOf (undefined::Int16)

writeSignalMonoBlock ::
   FilePath -> [Int16] -> IO ()
writeSignalMonoBlock fileName signal =
   withBinaryFile fileName WriteMode $
      \h -> let blocks = sliceVertical maxBlockSize signal
            in  allocaBytes (int16size * maxBlockSize) $
                   \p -> mapM_ (putIntBlock h p) blocks

putIntBlock :: Handle -> Ptr Int16 -> [Int16] -> IO ()
putIntBlock h p xs =
   do cnt <- foldM (\n x -> pokeElemOff p n x >> return (n+1)) 0 xs
      hPutBuf h p (int16size * cnt)

_putIntBlockSlow :: Handle -> Ptr Int16 -> [Int16] -> IO ()
_putIntBlockSlow h p xs =
   do zipWithM_ (pokeElemOff p) [0..] xs
      hPutBuf h p (int16size * length xs)


chopLength :: Int {- ^ block size -} -> Int {- ^ length -} -> [Int]
chopLength blockSize =
   unfoldr (\l -> let chunkSize = min blockSize l
                  in  toMaybe (l>0) (chunkSize, l-chunkSize))

writeZeroBlocks ::
   FilePath -> Int -> IO ()
writeZeroBlocks fileName len =
   withBinaryFile fileName WriteMode $
      \h -> allocaBytes (int16size * maxBlockSize) $
         \p ->
             do mapM_ (\off -> pokeElemOff p off (P98.fromInteger 0 :: Int16))
                      [0 .. maxBlockSize-1]
                mapM_ (hPutBuf h p)
                      (map (int16size*) (chopLength maxBlockSize len))


{- * driver -}

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
numSamples = 200000

zeroSignal, sawSignal :: [Double]
zeroSignal = replicate numSamples 0
sawSignal  = take numSamples (osciModSaw 0 (exponential2 100000 0.1))

polysawSignal :: [Double]
polysawSignal =
   take numSamples
      (osciModSaw 0 (exponential2 100000 0.1) +
       osciModSaw 0 (exponential2 100000 0.1001))

zeroSignal16, sawSignal16 :: [Int16]
zeroSignal16 = map numToInt16 zeroSignal
sawSignal16  = map numToInt16 sawSignal

sawSignal16NonShared :: Double -> [Int16]
sawSignal16NonShared halfLife =
   map numToInt16
       (take numSamples (osciModSaw 0 (exponential2 halfLife 0.1) :: [Double]))

sawSignalIntNonShared :: Double -> [Int]
sawSignalIntNonShared halfLife =
   map doubleToInt
       (take numSamples (osciModSaw 0 (exponential2 halfLife 0.1) :: [Double]))

zeroStream, zeroStreamPaired :: String
zeroStream       = replicate (2*numSamples) '\000'
zeroStreamPaired = concat $ replicate numSamples "\001\000"

sawStream :: String
sawStream = take (2*numSamples) (cycle ['\000'..'\177'])

zeroByteString :: B.ByteString
zeroByteString =
   B.replicate (P98.fromIntegral (2 * numSamples))
      (P98.fromIntegral (0::Int))

zeroByteStringPaired :: B.ByteString
zeroByteStringPaired =
   B.concat $ replicate numSamples $
      B.pack [P98.fromIntegral (0::Int), P98.fromIntegral (1::Int)]


tests :: [(String, FilePath, FilePath -> IO ())]
tests =
   ("zero bytestring",        "zerobytestring.sw", flip B.writeFile zeroByteString) :
   ("zero bytestring words",  "zerobytestrnwd.sw", flip B.writeFile zeroByteStringPaired) :
   ("zero blocks",            "zerofastblocks.sw", flip writeZeroBlocks numSamples) :
   ("zero bytes",             "zerofast.sw",       flip writeFile zeroStream) :
   ("zero words",             "zerowords.sw",      flip writeFile zeroStreamPaired) :
   ("saw bytes",              "sawbytes.sw",       flip writeFile sawStream) :
   -- only the first test is reliable, because the subsequent test can access the already computed data
   ("zero signal binary put", "zerowordbinary.sw", flip writeSignalMonoBinaryPut zeroSignal16) :
   ("zero signal bytestring", "zerowordstring.sw", flip writeSignalMonoBStr  zeroSignal16) :
   ("zero signal block-wise", "zeroblock.sw",      flip writeSignalMonoBlock zeroSignal16) :
   ("zero signal poke",       "zeropoke.sw",       flip writeSignalMonoPoke  zeroSignal16) :
   ("zero signal foldr",      "zerofoldr.sw",      flip writeSignalMonoS     zeroSignal16) :
   ("zero signal",            "zero.sw",           flip writeSignalMono      zeroSignal16) :
   ("saw binary int lib",     "sawbinaryint.sw",   flip writeSignalMonoBinaryIntPut $ sawSignalIntNonShared 100004) :
   ("saw int",                "sawint.sw",         flip writeSignalMonoInt $ sawSignalIntNonShared 100005) :
   -- the same problem as with zeros
   ("saw bytestring",         "sawbytestring.sw",  flip writeSignalMono      sawSignal16) :
   ("saw",                    "saw.sw",            flip writeSignalMono      sawSignal16) :
   ("saw bytestring non-shd", "sawbytestrngns.sw", flip writeSignalMono      $ sawSignal16NonShared 100001) :
   ("saw non-shared",         "sawns.sw",          flip writeSignalMono      $ sawSignal16NonShared 100002) :
   ("saw binary lib",         "sawbinary.sw",      flip writeSignalMonoBinaryPut $ sawSignal16NonShared 100003) :
   ("poly-saw binary lib",    "polysawbinary.sw",  flip writeSignalMonoBinaryPut $ map numToInt16 polysawSignal) :
   []


main :: IO ()
main =
   do mapM_ (\(label, fileName, action) ->
              measureTime label (action fileName))
           tests

      when False $
         mapM_ (\(_,fileName,_) -> removeFile fileName)
           tests
