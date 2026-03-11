{- |
This is old code, handling Int16 using two characters.
-}
module Synthesizer.Plain.IO
   {-# DEPRECATED "Use Sound.Sox.Signal.List instead." #-}
   (
    writeInt16Stream, readInt16StreamStrict,
    writeLEInt16Stream, readLEInt16Stream,
    putInt16Stream, putInt16StreamChunky,
    -- historical functions
    intToTwoLEChars, twoLECharsToInt,
   ) where

import Foreign (Int16, Ptr, alloca, sizeOf, poke, peek)
import System.IO
          (openBinaryFile, IOMode(WriteMode,ReadMode), hClose,
           Handle, hPutBuf, hGetBuf)
import Control.Exception (bracket, )
import Control.Monad (liftM, )

import qualified Data.ByteString.Lazy as B
import qualified Data.Binary.Builder as Builder
import Data.Monoid (mconcat, )
import Data.Char (ord, )

import NumericPrelude.Numeric
import NumericPrelude.Base
import qualified Prelude as P98



-- | little endian (Intel)
{-# INLINE leCharsToInt16 #-}
leCharsToInt16 :: Char -> Char -> Int16
leCharsToInt16 hi lo =
   P98.fromIntegral $ ord lo + 256 * ord hi

twoLECharsToInt :: Char -> Char -> Int
twoLECharsToInt hi lo =
   let unsigned = ord lo + 256 * ord hi
   in  mod (unsigned + 32768) 65536 - 32768


-- | little endian (Intel)
{-# INLINE int16ToLEChars #-}
int16ToLEChars :: Int16 -> [Char]
int16ToLEChars x =
   let (hi,lo) = divMod (P98.fromIntegral x) 256
   in  [toEnum lo, toEnum (mod hi 256)]

intToTwoLEChars :: Int -> [Char]
intToTwoLEChars x =
   let (hi,lo) = divMod x 256
   in  [toEnum lo, toEnum (mod hi 256)]



{-# INLINE binaryToIntsMono16 #-}
binaryToIntsMono16 :: [Char] -> [Int16]
binaryToIntsMono16 sig =
   case sig of
      (lo:hi:xs) ->
         leCharsToInt16 hi lo : binaryToIntsMono16 xs
      (_:[]) ->
         error "binaryToIntsMono16: 16 bit sample files must have even length"
      [] -> []


{- |
Write a little endian 16 bit integer stream
via String data and 'writeFile'.
-}
writeLEInt16Stream :: FilePath -> [Int16] -> IO ()
writeLEInt16Stream fileName =
   writeFile fileName . concatMap int16ToLEChars

{- |
Uses endianess of the machine, like Sox does.
-}
writeInt16Stream :: FilePath -> [Int16] -> IO ()
writeInt16Stream fileName stream =
   bracket (openBinaryFile fileName WriteMode) hClose
      (flip putInt16Stream stream)

putInt16StreamChunky :: Handle -> [Int16] -> IO ()
putInt16StreamChunky h =
   B.hPut h . Builder.toLazyByteString .
   mconcat . map (Builder.putWord16host . P98.fromIntegral)

putInt16Stream :: Handle -> [Int16] -> IO ()
putInt16Stream h stream =
   alloca $
      \p -> mapM_ (putInt16 h p) stream

putInt16 :: Handle -> Ptr Int16 -> Int16 -> IO ()
putInt16 h p n =
   poke p n >> hPutBuf h p (sizeOf n)


{- |
The end of the list is undefined,
if the file has odd length.
It would be better if it throws an exception.
-}
readLEInt16Stream :: FilePath -> IO [Int16]
readLEInt16Stream fileName =
   fmap binaryToIntsMono16 (readFile fileName)

{- |
The end of the list is undefined,
if the file has odd length.
It would be better if it throws an exception.
-}
readInt16StreamStrict :: FilePath -> IO [Int16]
readInt16StreamStrict fileName =
   bracket (openBinaryFile fileName ReadMode) hClose
      getInt16StreamStrict

getInt16StreamStrict :: Handle -> IO [Int16]
getInt16StreamStrict h =
   alloca $
      \p -> fmap (map P98.fromIntegral)
                 (unfoldM (getInt16 h p))

-- candidate for Utility
unfoldM :: Monad m => m (Maybe a) -> m [a]
unfoldM act =
   let listM = maybe (return []) (\x -> liftM (x:) listM) =<< act
   in  listM

getInt16 :: Handle -> Ptr Int16 -> IO (Maybe Int16)
getInt16 h p =
   do cnt <- hGetBuf h p (sizeOf (undefined::Int16))
      case cnt of
        0 -> return Nothing
        2 -> fmap Just (peek p)
        _ -> return (error "getInt16: only one byte found")
