module Sound.MIDI.Writer.Basic where

import qualified Numeric.NonNegative.Wrapper as NonNeg

import qualified Sound.MIDI.Bit as Bit
import qualified Sound.MIDI.IO as MIO
import qualified Data.Monoid as Monoid

import Data.Bits ((.|.))
import Sound.MIDI.IO (listByteFromChar, )
import Sound.MIDI.Monoid ((+#+), genAppend, genConcat, nonEmptyConcat, )
import Data.Foldable (foldMap, )
import Data.Monoid (Monoid, mempty, mappend, mconcat, )
import Data.Semigroup (Semigroup(sconcat, (<>)), )

import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask, )
import Control.Monad.Trans.Class (lift, )

import Data.List (genericLength, )
import Data.Word (Word8, )
import Data.Char (chr, )

import qualified Data.ByteString.Lazy as B
import qualified Data.Binary.Builder as Builder
import Data.Binary.Builder (Builder, fromLazyByteString, )

import Control.Exception (bracket, )
import qualified System.IO as IO
import System.IO (openBinaryFile, hClose, hPutChar, Handle, IOMode(WriteMode))

import Prelude hiding (putStr, )



class Monoid m => C m where
   putByte :: Word8 -> m
   {- |
   @putLengthBlock n writeBody@
   write @n@ bytes indicating the number of bytes written by @writeBody@
   and then it runs @writeBody@.
   -}
   putLengthBlock :: Int -> m -> m


-- differences list
newtype ByteList = ByteList {unByteList :: Monoid.Endo MIO.ByteList}

instance Semigroup ByteList where
   (<>) = genAppend ByteList unByteList
   sconcat = nonEmptyConcat ByteList unByteList

instance Monoid ByteList where
   mempty = ByteList mempty
   mappend = (<>)
   mconcat = genConcat ByteList unByteList

instance C ByteList where
   putByte = ByteList . Monoid.Endo . (:)
   putLengthBlock n writeBody =
      let body = runByteList writeBody
      in  putInt n (length body) `mappend`
          putByteListSpec body  -- we could call 'writeBody' but this would recompute the data


-- | 'putByteList' specialised to 'ByteList'
putByteListSpec :: MIO.ByteList -> ByteList
putByteListSpec = ByteList . Monoid.Endo . (++)


runByteList :: ByteList -> MIO.ByteList
runByteList = flip Monoid.appEndo [] . unByteList




newtype ByteString = ByteString {unByteString :: Builder}

instance Semigroup ByteString where
   (<>) = genAppend ByteString unByteString
   sconcat = nonEmptyConcat ByteString unByteString

instance Monoid ByteString where
   mempty = ByteString $ mempty
   mappend = (<>)
   mconcat = genConcat ByteString unByteString

instance C ByteString where
   putByte = ByteString . Builder.singleton
   putLengthBlock n writeBody =
      let body = runByteString writeBody
          len = B.length body
          errLen =
             if len >= div (256^n) 2
               then error "Chunk too large"
               else fromIntegral len
      in  putInt n errLen +#+ ByteString (fromLazyByteString body)


runByteString :: ByteString -> B.ByteString
runByteString = Builder.toLazyByteString . unByteString




newtype SeekableFile = SeekableFile {unSeekableFile :: ReaderT Handle IO ()}

instance Semigroup SeekableFile where
   x <> y = SeekableFile $ unSeekableFile x >> unSeekableFile y

instance Monoid SeekableFile where
   mempty = SeekableFile $ return ()
   mappend = (<>)
   mconcat = SeekableFile . mapM_ unSeekableFile

instance C SeekableFile where
   putByte c =
      SeekableFile $
         ask >>= \h ->
         lift $ hPutChar h (chr $ fromIntegral c)
   putLengthBlock n writeBody =
      SeekableFile $
      ask >>= \h -> lift $
      do lenPos <- IO.hGetPosn h
         IO.hPutStr h (replicate n '\000')
         startPos <- IO.hTell h
         runSeekableHandle h writeBody
         stopPos <- IO.hTell h
         contPos <- IO.hGetPosn h
         IO.hSetPosn lenPos
         let len = stopPos - startPos
         if len >= 2^(31::Int)
           then ioError (userError ("chunk too large, size " ++ show len))
           else runSeekableHandle h (putInt n (fromInteger len))
         IO.hSetPosn contPos


runSeekableFile :: FilePath -> SeekableFile -> IO ()
runSeekableFile name w =
   bracket
      (openBinaryFile name WriteMode)
      hClose
      (flip runSeekableHandle w)

runSeekableHandle :: Handle -> SeekableFile -> IO ()
runSeekableHandle h w =
   runReaderT (unSeekableFile w) h




putInt :: C writer => Int -> Int -> writer
putInt a = putByteList . map fromIntegral . Bit.someBytes a

putStr :: C writer => String -> writer
putStr = putByteList . listByteFromChar

putIntAsByte :: C writer => Int -> writer
putIntAsByte x = putByte $ fromIntegral x

putByteList :: C writer => MIO.ByteList -> writer
putByteList = foldMap putByte

putLenByteList :: C writer => MIO.ByteList -> writer
putLenByteList bytes =
   putVar (genericLength bytes) +#+
   putByteList bytes

{- |
Numbers of variable size are represented by sequences of 7-bit blocks
tagged (in the top bit) with a bit indicating:
(1) that more data follows; or
(0) that this is the last block.
-}

putVar :: C writer => NonNeg.Integer -> writer
putVar n =
   let bytes = map fromIntegral $ Bit.toBase 128 n
   in  case bytes of
          [] -> putInt 1 0
          (_:bs) ->
             let highBits = map (const 128) bs ++ [0]
             in  putByteList (zipWith (.|.) highBits bytes)
