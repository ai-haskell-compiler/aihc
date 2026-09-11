-- | C strings. The foreign encoding of this library is UTF-8, so the
-- @CString@ functions encode and decode UTF-8; the @CAString@ functions
-- truncate each character to a byte, as in base.
module Foreign.C.String
  ( CString,
    CStringLen,
    peekCString,
    peekCStringLen,
    newCString,
    newCStringLen,
    withCString,
    withCStringLen,
    charIsRepresentable,
    castCharToCChar,
    castCCharToChar,
    castCharToCUChar,
    castCUCharToChar,
    castCharToCSChar,
    castCSCharToChar,
    peekCAString,
    peekCAStringLen,
    newCAString,
    newCAStringLen,
    withCAString,
    withCAStringLen,
    CWString,
    CWStringLen,
  )
where

import Data.Word (Word8)
import Foreign.C.Types (CChar (..), CSChar (..), CUChar (..), CWchar)
import Foreign.Marshal.Alloc (allocaBytes, mallocBytes)
import Foreign.Marshal.Array (peekArray, peekArray0, pokeArray, pokeArray0)
import Foreign.Ptr (Ptr, castPtr)
import GHC.Base (ord)
import GHC.Char (chr)
import GHC.Internal.Utf8 (decodeUtf8, encodeUtf8)
import Prelude

type CString = Ptr CChar

type CStringLen = (Ptr CChar, Int)

type CWString = Ptr CWchar

type CWStringLen = (Ptr CWchar, Int)

-- | Read a NUL-terminated UTF-8 string.
peekCString :: CString -> IO String
peekCString pointer = fmap decodeBytes (peekBytes0 pointer)

-- | Read a UTF-8 string of the given byte length.
peekCStringLen :: CStringLen -> IO String
peekCStringLen (pointer, byteCount) = fmap decodeBytes (peekBytes pointer byteCount)

-- | Allocate a NUL-terminated UTF-8 copy of the string with 'mallocBytes'.
newCString :: String -> IO CString
newCString string = newBytes0 (encodeBytes string)

-- | Allocate a UTF-8 copy of the string with 'mallocBytes' and return its
-- byte length.
newCStringLen :: String -> IO CStringLen
newCStringLen string = newBytes (encodeBytes string)

-- | Run an action on a temporary NUL-terminated UTF-8 copy of the string.
withCString :: String -> (CString -> IO a) -> IO a
withCString string = withBytes0 (encodeBytes string)

-- | Run an action on a temporary UTF-8 copy of the string and its byte
-- length.
withCStringLen :: String -> (CStringLen -> IO a) -> IO a
withCStringLen string = withBytes (encodeBytes string)

-- | Whether the foreign encoding can represent the character. UTF-8
-- represents every character.
charIsRepresentable :: Char -> IO Bool
charIsRepresentable _ = return True

castCharToCChar :: Char -> CChar
castCharToCChar character = CChar (fromIntegral (ord character))

castCCharToChar :: CChar -> Char
castCCharToChar (CChar value) = chr (fromIntegral (fromIntegral value :: Word8))

castCharToCUChar :: Char -> CUChar
castCharToCUChar character = CUChar (fromIntegral (ord character))

castCUCharToChar :: CUChar -> Char
castCUCharToChar (CUChar value) = chr (fromIntegral value)

castCharToCSChar :: Char -> CSChar
castCharToCSChar character = CSChar (fromIntegral (ord character))

castCSCharToChar :: CSChar -> Char
castCSCharToChar (CSChar value) = chr (fromIntegral (fromIntegral value :: Word8))

-- | Read a NUL-terminated string one byte per character.
peekCAString :: CString -> IO String
peekCAString pointer = fmap (map byteToChar) (peekBytes0 pointer)

-- | Read a string of the given length one byte per character.
peekCAStringLen :: CStringLen -> IO String
peekCAStringLen (pointer, byteCount) = fmap (map byteToChar) (peekBytes pointer byteCount)

-- | Allocate a NUL-terminated copy of the string, one byte per character.
newCAString :: String -> IO CString
newCAString string = newBytes0 (map charToByte string)

-- | Allocate a copy of the string, one byte per character, and return its
-- length.
newCAStringLen :: String -> IO CStringLen
newCAStringLen string = newBytes (map charToByte string)

-- | Run an action on a temporary NUL-terminated copy of the string, one
-- byte per character.
withCAString :: String -> (CString -> IO a) -> IO a
withCAString string = withBytes0 (map charToByte string)

-- | Run an action on a temporary copy of the string, one byte per
-- character, and its length.
withCAStringLen :: String -> (CStringLen -> IO a) -> IO a
withCAStringLen string = withBytes (map charToByte string)

encodeBytes :: String -> [Word8]
encodeBytes string = map fromIntegral (encodeUtf8 string)

decodeBytes :: [Word8] -> String
decodeBytes bytes = decodeUtf8 (map fromIntegral bytes)

byteToChar :: Word8 -> Char
byteToChar byte = chr (fromIntegral byte)

charToByte :: Char -> Word8
charToByte character = fromIntegral (ord character)

peekBytes0 :: Ptr a -> IO [Word8]
peekBytes0 pointer = peekArray0 0 (castPtr pointer)

peekBytes :: Ptr a -> Int -> IO [Word8]
peekBytes pointer byteCount = peekArray byteCount (castPtr pointer)

newBytes0 :: [Word8] -> IO (Ptr a)
newBytes0 bytes = do
  pointer <- mallocBytes (length bytes + 1)
  pokeArray0 0 pointer bytes
  return (castPtr pointer)

newBytes :: [Word8] -> IO (Ptr a, Int)
newBytes bytes = do
  let byteCount = length bytes
  pointer <- mallocBytes byteCount
  pokeArray pointer bytes
  return (castPtr pointer, byteCount)

withBytes0 :: [Word8] -> (Ptr a -> IO b) -> IO b
withBytes0 bytes action =
  allocaBytes (length bytes + 1) $ \pointer -> do
    pokeArray0 0 pointer bytes
    action (castPtr pointer)

withBytes :: [Word8] -> ((Ptr a, Int) -> IO b) -> IO b
withBytes bytes action =
  let byteCount = length bytes
   in allocaBytes byteCount $ \pointer -> do
        pokeArray pointer bytes
        action (castPtr pointer, byteCount)
