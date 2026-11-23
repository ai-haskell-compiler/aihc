-- | String marshalling through text encodings.
--
-- A 'TextEncoding' here is a name only. Thus, this module chooses a codec
-- from the name: UTF-8, UTF-16, or UTF-32, as in base. Each other name
-- gives one byte for each character, as the @CAString@ functions of
-- "Foreign.C.String" do.
--
-- A UTF-16 or UTF-32 string has a terminator of one code unit, with all
-- bytes zero. The encoding @UTF-16@ or @UTF-32@ with no byte order writes
-- a byte order mark and then big-endian code units, as base does. When it
-- reads, a byte order mark selects the byte order, and the default is
-- big-endian.
module GHC.Foreign
  ( peekCString,
    peekCStringLen,
    newCString,
    newCStringLen,
    withCString,
    withCStringLen,
  )
where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Word (Word8)
import Foreign.C.String (CString, CStringLen)
import Foreign.C.String qualified as C
import Foreign.Marshal.Alloc (allocaBytes, mallocBytes)
import Foreign.Marshal.Array (peekArray, pokeArray)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import GHC.Char (chr)
import GHC.IO.Encoding (TextEncoding, textEncodingName)
import GHC.Internal.Base (ord)
import Prelude

-- | The byte order of a code unit.
data ByteOrder = BigEndian | LittleEndian

-- | The codec of an encoding.
data Codec
  = Utf8
  | -- | A codec with code units of a number of bytes: 2 for UTF-16 and 4
    -- for UTF-32. With no byte order, the codec has a byte order mark.
    Units Int (Maybe ByteOrder)
  | -- | One byte for each character.
    Bytes

-- | The codec of an encoding. The name can have the suffix of a failure
-- mode, such as @//IGNORE@.
codecOf :: TextEncoding -> Codec
codecOf encoding =
  case takeWhile (/= '/') (textEncodingName encoding) of
    "UTF-8" -> Utf8
    "UTF-8BOM" -> Utf8
    "UTF8" -> Utf8
    "UTF-16" -> Units 2 Nothing
    "UTF-16BE" -> Units 2 (Just BigEndian)
    "UTF-16LE" -> Units 2 (Just LittleEndian)
    "UTF-32" -> Units 4 Nothing
    "UTF-32BE" -> Units 4 (Just BigEndian)
    "UTF-32LE" -> Units 4 (Just LittleEndian)
    _ -> Bytes

peekCString :: TextEncoding -> CString -> IO String
peekCString encoding pointer =
  case codecOf encoding of
    Utf8 -> C.peekCString pointer
    Bytes -> C.peekCAString pointer
    Units size order -> do
      count <- unitCount size pointer 0
      decodeUnits size order <$> peekArray (count * size) (castPtr pointer)

peekCStringLen :: TextEncoding -> CStringLen -> IO String
peekCStringLen encoding (pointer, byteCount) =
  case codecOf encoding of
    Utf8 -> C.peekCStringLen (pointer, byteCount)
    Bytes -> C.peekCAStringLen (pointer, byteCount)
    Units size order -> decodeUnits size order <$> peekArray byteCount (castPtr pointer)

newCString :: TextEncoding -> String -> IO CString
newCString encoding string =
  case codecOf encoding of
    Utf8 -> C.newCString string
    Bytes -> C.newCAString string
    Units size order -> do
      let bytes = encodeUnits size order string ++ replicate size 0
      pointer <- mallocBytes (length bytes)
      pokeArray pointer bytes
      pure (castPtr pointer)

newCStringLen :: TextEncoding -> String -> IO CStringLen
newCStringLen encoding string =
  case codecOf encoding of
    Utf8 -> C.newCStringLen string
    Bytes -> C.newCAStringLen string
    Units size order -> do
      let bytes = encodeUnits size order string
          byteCount = length bytes
      pointer <- mallocBytes byteCount
      pokeArray pointer bytes
      pure (castPtr pointer, byteCount)

withCString :: TextEncoding -> String -> (CString -> IO a) -> IO a
withCString encoding string action =
  case codecOf encoding of
    Utf8 -> C.withCString string action
    Bytes -> C.withCAString string action
    Units size order -> do
      let bytes = encodeUnits size order string ++ replicate size 0
      allocaBytes (length bytes) $ \pointer -> do
        pokeArray pointer bytes
        action (castPtr pointer)

withCStringLen :: TextEncoding -> String -> (CStringLen -> IO a) -> IO a
withCStringLen encoding string action =
  case codecOf encoding of
    Utf8 -> C.withCStringLen string action
    Bytes -> C.withCAStringLen string action
    Units size order -> do
      let bytes = encodeUnits size order string
          byteCount = length bytes
      allocaBytes byteCount $ \pointer -> do
        pokeArray pointer bytes
        action (castPtr pointer, byteCount)

-- | The number of code units before the terminator.
unitCount :: Int -> Ptr a -> Int -> IO Int
unitCount size pointer count = do
  unit <- peekArray size (castPtr (pointer `plusPtr` (count * size)) :: Ptr Word8)
  if all (== 0) unit then pure count else unitCount size pointer (count + 1)

-- | The bytes of a string in code units of a size.
encodeUnits :: Int -> Maybe ByteOrder -> String -> [Word8]
encodeUnits size order string =
  case order of
    Just byteOrder -> concatMap (unitBytes byteOrder) (codeUnits string)
    Nothing -> concatMap (unitBytes BigEndian) (0xFEFF : codeUnits string)
  where
    codeUnits = concatMap (if size == 2 then utf16Units . ord else pure . ord)
    unitBytes byteOrder unit =
      let bigEndian = [fromIntegral (unit `shiftR` (8 * index)) | index <- [size - 1, size - 2 .. 0]]
       in case byteOrder of
            BigEndian -> bigEndian
            LittleEndian -> reverse bigEndian

-- | The UTF-16 code units of a code point.
utf16Units :: Int -> [Int]
utf16Units code
  | code < 0x10000 = [code]
  | otherwise =
      let offset = code - 0x10000
       in [0xD800 .|. (offset `shiftR` 10), 0xDC00 .|. (offset .&. 0x3FF)]

-- | The string of bytes in code units of a size. Bytes after the last
-- complete code unit are not part of the string.
decodeUnits :: Int -> Maybe ByteOrder -> [Word8] -> String
decodeUnits size order bytes =
  case (order, units BigEndian bytes) of
    (Just byteOrder, _) -> characters (units byteOrder bytes)
    (Nothing, 0xFEFF : rest) -> characters rest
    (Nothing, _) ->
      case units LittleEndian bytes of
        0xFEFF : rest -> characters rest
        _ -> characters (units BigEndian bytes)
  where
    units byteOrder remaining =
      case splitAt size remaining of
        (unit, rest)
          | length unit == size -> unitValue byteOrder unit : units byteOrder rest
          | otherwise -> []
    unitValue byteOrder unit =
      let ordered = case byteOrder of
            BigEndian -> unit
            LittleEndian -> reverse unit
       in foldl (\value byte -> (value `shiftL` 8) .|. fromIntegral byte) 0 ordered
    characters
      | size == 2 = utf16Characters
      | otherwise = map codePoint
    codePoint code
      | code <= 0x10FFFF = chr code
      | otherwise = '\xFFFD'

-- | The characters of UTF-16 code units. A surrogate pair is one
-- character.
utf16Characters :: [Int] -> String
utf16Characters units =
  case units of
    high : low : rest
      | high >= 0xD800 && high < 0xDC00 && low >= 0xDC00 && low < 0xE000 ->
          chr (0x10000 + ((high - 0xD800) `shiftL` 10) + (low - 0xDC00)) : utf16Characters rest
    unit : rest -> chr unit : utf16Characters rest
    [] -> []
