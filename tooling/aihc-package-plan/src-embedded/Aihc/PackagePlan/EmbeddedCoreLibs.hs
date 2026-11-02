-- |
-- Module      : Aihc.PackagePlan.EmbeddedCoreLibs
-- Description : The core-libs sources in the executable
--
-- A release build puts the files under @core-libs@ into the executable, so
-- that the compiler needs no checkout and no download to find its core
-- libraries. The @embed-core-libs@ flag selects this module.
--
-- @scripts/embed-core-libs.sh@ writes a C file with one lz4 frame. This
-- module reads that frame from the C symbols and decodes it. Thus the build
-- needs no Template Haskell. The frame holds an archive. Each file in the
-- archive is its relative path, a NUL byte, its size in decimal, a NUL
-- byte, and its contents.
module Aihc.PackagePlan.EmbeddedCoreLibs
  ( embeddedCoreLibs,
  )
where

import Control.Monad (unless, when)
import Control.Monad.ST (ST, runST)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.Bits (Bits, shiftL, shiftR, testBit, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Short qualified as SBS
import Data.Primitive.ByteArray
  ( MutableByteArray,
    copyMutableByteArray,
    copyPtrToMutableByteArray,
    newByteArray,
    readByteArray,
    unsafeFreezeByteArray,
    writeByteArray,
  )
import Data.Primitive.Ptr (indexOffPtr)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word32, Word64, Word8)
import Foreign.Ptr (Ptr, plusPtr)

foreign import ccall "&aihc_core_libs_lz4" frameStart :: Ptr Word8

foreign import ccall "&aihc_core_libs_lz4_size" frameSizeCell :: Ptr Word64

-- | Each file under @core-libs@, with its path relative to that directory.
embeddedCoreLibs :: Maybe [(FilePath, ByteString)]
embeddedCoreLibs =
  case decodeFrame frameStart (fromIntegral (indexOffPtr frameSizeCell 0)) >>= splitArchive of
    Right files -> Just files
    Left message -> error ("The core-libs sources in the executable are not valid: " <> message)

-- | Decode an lz4 frame of the given size. The frame must give its content
-- size. The decoder does not examine the checksums, because the frame is in
-- the executable and does not come from outside.
decodeFrame :: Ptr Word8 -> Int -> Either String ByteString
decodeFrame frame frameSize = do
  unless (frameSize >= 15) (Left "the lz4 frame is too short")
  unless (word32At 0 == 0x184D2204) (Left "the lz4 frame has no magic number")
  let flags = byteAt 4
  unless (flags `shiftR` 6 == 1) (Left "the lz4 frame version is not 1")
  unless (testBit flags 3) (Left "the lz4 frame has no content size")
  when (testBit flags 0) (Left "the lz4 frame needs a dictionary")
  let contentSize = fromIntegral (word64At 6)
      blockChecksumSize = if testBit flags 4 then 4 else 0
  runST $ runExceptT $ do
    output <- lift (newByteArray contentSize)
    written <- decodeBlocks output contentSize blockChecksumSize 15 0
    unless (written == contentSize) (throwE "the lz4 content size is not correct")
    bytes <- lift (unsafeFreezeByteArray output)
    pure (SBS.fromShort (SBS.ShortByteString bytes))
  where
    byteAt :: Int -> Word8
    byteAt = indexOffPtr frame

    word32At :: Int -> Word32
    word32At position = littleEndian [byteAt (position + index) | index <- [0 .. 3]]

    word64At :: Int -> Word64
    word64At position = littleEndian [byteAt (position + index) | index <- [0 .. 7]]

    littleEndian :: (Integral a, Num b, Bits b) => [a] -> b
    littleEndian = foldr (\byte rest -> fromIntegral byte .|. (rest `shiftL` 8)) 0

    decodeBlocks :: MutableByteArray s -> Int -> Int -> Int -> Int -> ExceptT String (ST s) Int
    decodeBlocks output capacity checksumSize position written = do
      unless (position + 4 <= frameSize) (throwE "the lz4 frame has no end mark")
      let header = word32At position
          blockSize = fromIntegral (header .&. 0x7FFFFFFF)
          start = position + 4
          end = start + blockSize
      if header == 0
        then pure written
        else do
          unless (end + checksumSize <= frameSize) (throwE "an lz4 block is too long")
          next <-
            if testBit header 31
              then copyLiterals output capacity start blockSize written
              else decodeSequences output capacity start end written
          decodeBlocks output capacity checksumSize (end + checksumSize) next

    copyLiterals :: MutableByteArray s -> Int -> Int -> Int -> Int -> ExceptT String (ST s) Int
    copyLiterals output capacity start count written = do
      unless (written + count <= capacity) (throwE "the lz4 content is too long")
      lift (copyPtrToMutableByteArray output written (frame `plusPtr` start :: Ptr Word8) count)
      pure (written + count)

    -- One lz4 block is a list of sequences. A sequence is literals and then
    -- a match. The last sequence has only literals.
    decodeSequences :: MutableByteArray s -> Int -> Int -> Int -> Int -> ExceptT String (ST s) Int
    decodeSequences output capacity input end written = do
      unless (input < end) (throwE "an lz4 block has no last sequence")
      let token = byteAt input
      (literalCount, literalStart) <- extendedLength (fromIntegral (token `shiftR` 4)) (input + 1) end
      let literalEnd = literalStart + literalCount
      unless (literalEnd <= end) (throwE "lz4 literals go past the block")
      afterLiterals <- copyLiterals output capacity literalStart literalCount written
      if literalEnd == end
        then pure afterLiterals
        else do
          unless (literalEnd + 2 <= end) (throwE "an lz4 match has no offset")
          let offset = fromIntegral (byteAt literalEnd) .|. (fromIntegral (byteAt (literalEnd + 1)) `shiftL` 8)
          unless (offset /= 0 && offset <= afterLiterals) (throwE "an lz4 match offset is not valid")
          (matchExtra, next) <- extendedLength (fromIntegral (token .&. 0x0F)) (literalEnd + 2) end
          let matchCount = matchExtra + 4
              source = afterLiterals - offset
          unless (afterLiterals + matchCount <= capacity) (throwE "the lz4 content is too long")
          lift $
            if offset >= matchCount
              then copyMutableByteArray output afterLiterals output source matchCount
              else copyOverlapping output source afterLiterals matchCount
          decodeSequences output capacity next end (afterLiterals + matchCount)

    -- A length of 15 in a token continues in the next bytes. Each byte adds
    -- its value, and a byte of 255 tells that one more byte follows.
    extendedLength :: Int -> Int -> Int -> ExceptT String (ST s) (Int, Int)
    extendedLength value position end
      | value /= 15 = pure (value, position)
      | otherwise = go value position
      where
        go total current = do
          unless (current < end) (throwE "an lz4 length goes past the block")
          let byte = byteAt current
              next = total + fromIntegral byte
          if byte == 255 then go next (current + 1) else pure (next, current + 1)

-- | Copy a match that overlaps its own output one byte at a time, because
-- each byte can be a copy of a byte that this match wrote.
copyOverlapping :: MutableByteArray s -> Int -> Int -> Int -> ST s ()
copyOverlapping output source target count =
  mapM_ copyByte [0 .. count - 1]
  where
    copyByte index = do
      byte <- readByteArray output (source + index)
      writeByteArray output (target + index) (byte :: Word8)

-- | Split the archive into its files.
splitArchive :: ByteString -> Either String [(FilePath, ByteString)]
splitArchive bytes
  | BS.null bytes = Right []
  | otherwise = do
      (path, afterPath) <- field bytes
      (sizeText, afterSize) <- field afterPath
      size <- case BS8.readInt sizeText of
        Just (size, rest) | BS.null rest, size >= 0 -> Right size
        _ -> Left "an archive file size is not a number"
      unless (size <= BS.length afterSize) (Left "an archive file goes past the archive")
      name <- either (const (Left "an archive path is not UTF-8")) (Right . T.unpack) (TE.decodeUtf8' path)
      let (contents, rest) = BS.splitAt size afterSize
      ((name, contents) :) <$> splitArchive rest
  where
    field input = case BS.elemIndex 0 input of
      Just index -> Right (BS.take index input, BS.drop (index + 1) input)
      Nothing -> Left "an archive field has no NUL byte"
