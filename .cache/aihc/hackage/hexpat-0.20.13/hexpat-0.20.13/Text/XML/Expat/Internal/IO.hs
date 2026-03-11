{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}


-- | Low-level interface to Expat. Unless speed is paramount, this should
-- normally be avoided in favour of the interfaces provided by
-- 'Text.XML.Expat.SAX' and 'Text.XML.Expat.Tree', etc.
module Text.XML.Expat.Internal.IO (
  HParser,
  hexpatNewParser,
  encodingToString,
  Encoding(..),
  XMLParseError(..),
  XMLParseLocation(..)
  ) where

import Control.Applicative
import Control.DeepSeq
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as I
import Data.Int
import Data.Word
import Foreign
import Foreign.C


data Parser_struct
type ParserPtr = Ptr Parser_struct

data Encoding = ASCII | UTF8 | UTF16 | ISO88591
encodingToString :: Encoding -> String
encodingToString ASCII    = "US-ASCII"
encodingToString UTF8     = "UTF-8"
encodingToString UTF16    = "UTF-16"
encodingToString ISO88591 = "ISO-8859-1"

withOptEncoding :: Maybe Encoding -> (CString -> IO a) -> IO a
withOptEncoding Nothing    f = f nullPtr
withOptEncoding (Just enc) f = withCString (encodingToString enc) f

-- ByteString.useAsCStringLen is almost what we need, but C2HS wants a CInt
-- instead of an Int.
withBStringLen :: B.ByteString -> ((CString, CInt) -> IO a) -> IO a
withBStringLen bs f = do
  B.useAsCStringLen bs $ \(str, len) -> f (str, fromIntegral len)

unStatus :: CInt -> Bool
unStatus 0 = False
unStatus _ = True

getError :: ParserPtr -> IO XMLParseError
getError pp = do
    code <- xmlGetErrorCode pp
    cerr <- xmlErrorString code
    err <- peekCString cerr
    loc <- getParseLocation pp
    return $ XMLParseError err loc

-- |Obtain C value from Haskell 'Bool'.
--
cFromBool :: Num a => Bool -> a
cFromBool = fromBool

-- | Parse error, consisting of message text and error location
data XMLParseError = XMLParseError String XMLParseLocation deriving (Eq, Show)

instance NFData XMLParseError where
    rnf (XMLParseError msg loc) = rnf (msg, loc)

-- | Specifies a location of an event within the input text
data XMLParseLocation = XMLParseLocation {
        xmlLineNumber   :: Int64,  -- ^ Line number of the event
        xmlColumnNumber :: Int64,  -- ^ Column number of the event
        xmlByteIndex    :: Int64,  -- ^ Byte index of event from start of document
        xmlByteCount    :: Int64   -- ^ The number of bytes in the event
    }
    deriving (Eq, Show)

instance NFData XMLParseLocation where
    rnf (XMLParseLocation lin col ind cou) = rnf (lin, col, ind, cou)

getParseLocation :: ParserPtr -> IO XMLParseLocation
getParseLocation pp = do
    line <- xmlGetCurrentLineNumber pp
    col <- xmlGetCurrentColumnNumber pp
    index <- xmlGetCurrentByteIndex pp
    count <- xmlGetCurrentByteCount pp
    return $ XMLParseLocation {
            xmlLineNumber = fromIntegral line,
            xmlColumnNumber = fromIntegral col,
            xmlByteIndex = fromIntegral index,
            xmlByteCount = fromIntegral count
        }

-- Note on word sizes:
--
-- on expat 2.0:
-- XML_GetCurrentLineNumber returns XML_Size
-- XML_GetCurrentColumnNumber returns XML_Size
-- XML_GetCurrentByteIndex returns XML_Index
-- These are defined in expat_external.h
--
-- debian-i386 says XML_Size and XML_Index are 4 bytes.
-- ubuntu-amd64 says XML_Size and XML_Index are 8 bytes.
-- These two systems do NOT define XML_LARGE_SIZE, which would force these types
-- to be 64-bit.
--
-- If we guess the word size too small, it shouldn't matter: We will just discard
-- the most significant part.  If we get the word size too large, we will get
-- garbage (very bad).
--
-- So - what I will do is use CLong and CULong, which correspond to what expat
-- is using when XML_LARGE_SIZE is disabled, and give the correct sizes on the
-- two machines mentioned above.  At the absolute worst the word size will be too
-- short.

foreign import ccall unsafe "expat.h XML_GetErrorCode" xmlGetErrorCode
    :: ParserPtr -> IO CInt
foreign import ccall unsafe "expat.h XML_GetCurrentLineNumber" xmlGetCurrentLineNumber
    :: ParserPtr -> IO CULong
foreign import ccall unsafe "expat.h XML_GetCurrentColumnNumber" xmlGetCurrentColumnNumber
    :: ParserPtr -> IO CULong
foreign import ccall unsafe "expat.h XML_GetCurrentByteIndex" xmlGetCurrentByteIndex
    :: ParserPtr -> IO CLong
foreign import ccall unsafe "expat.h XML_GetCurrentByteCount" xmlGetCurrentByteCount
    :: ParserPtr -> IO CInt
foreign import ccall unsafe "expat.h XML_ErrorString" xmlErrorString
    :: CInt -> IO CString

type HParser = B.ByteString -> Bool -> IO (ForeignPtr Word8, CInt, Maybe XMLParseError)

foreign import ccall unsafe "hexpatNewParser"
  _hexpatNewParser :: Ptr CChar -> CInt -> IO MyParserPtr

foreign import ccall unsafe "hexpatGetParser"
  _hexpatGetParser :: MyParserPtr -> ParserPtr

data MyParser_struct
type MyParserPtr = Ptr MyParser_struct

foreign import ccall "&hexpatFreeParser" hexpatFreeParser :: FunPtr (MyParserPtr -> IO ())

hexpatNewParser :: Maybe Encoding
                -> Maybe (B.ByteString -> Maybe B.ByteString)  -- ^ Entity decoder
                -> Bool        -- ^ Whether to include input locations
                -> IO (HParser, IO XMLParseLocation)
hexpatNewParser enc mDecoder locations =
    withOptEncoding enc $ \cEnc -> do
        parser <- newForeignPtr hexpatFreeParser =<< _hexpatNewParser cEnc (cFromBool locations)
        return (parse parser, withForeignPtr parser $ \mp -> getParseLocation $ _hexpatGetParser mp)
  where
    parse parser = case mDecoder of
        Nothing -> \text final ->
            alloca $ \ppData ->
            alloca $ \pLen ->
            withBStringLen text $ \(textBuf, textLen) ->
            withForeignPtr parser $ \pp -> do
                ok <- unStatus <$> _hexpatParseUnsafe pp textBuf textLen (cFromBool final) ppData pLen
                pData <- peek ppData
                len <- peek pLen
                err <- if ok
                    then return Nothing
                    else Just <$> getError (_hexpatGetParser pp)
                fpData <- newForeignPtr funPtrFree pData
                return (fpData, len, err)
        Just decoder -> \text final ->
            alloca $ \ppData ->
            alloca $ \pLen ->
            withBStringLen text $ \(textBuf, textLen) ->
            withForeignPtr parser $ \pp -> do
                eh <- mkCEntityHandler . wrapCEntityHandler $ decoder
                _hexpatSetEntityHandler pp eh
                ok <- unStatus <$> _hexpatParseSafe pp textBuf textLen (cFromBool final) ppData pLen
                freeHaskellFunPtr eh
                pData <- peek ppData
                len <- peek pLen
                err <- if ok
                    then return Nothing
                    else Just <$> getError (_hexpatGetParser pp)
                fpData <- newForeignPtr funPtrFree pData
                return (fpData, len, err)

foreign import ccall unsafe "hexpatParse"
  _hexpatParseUnsafe :: MyParserPtr -> Ptr CChar -> CInt -> CInt -> Ptr (Ptr Word8) -> Ptr CInt -> IO CInt

foreign import ccall safe "hexpatParse"
  _hexpatParseSafe :: MyParserPtr -> Ptr CChar -> CInt -> CInt -> Ptr (Ptr Word8) -> Ptr CInt -> IO CInt

type CEntityHandler = Ptr CChar -> IO (Ptr CChar)

foreign import ccall safe "wrapper"
  mkCEntityHandler :: CEntityHandler
                   -> IO (FunPtr CEntityHandler)

peekByteStringLen :: CStringLen -> IO B.ByteString
{-# INLINE peekByteStringLen #-}
peekByteStringLen (cstr, len) =
    I.create (fromIntegral len) $ \ptr ->
        I.memcpy ptr (castPtr cstr) (fromIntegral len)

wrapCEntityHandler :: (B.ByteString -> Maybe B.ByteString) -> CEntityHandler
wrapCEntityHandler handler = h
  where
    h cname = do
        sz <- fromIntegral <$> I.c_strlen cname
        name <- peekByteStringLen (cname, sz)
        case handler name of
            Just text -> do
                let (fp, offset, len) = I.toForeignPtr text
                withForeignPtr fp $ \ctextBS -> do
                    ctext <- mallocBytes (len + 1) :: IO CString
                    I.memcpy (castPtr ctext) (ctextBS `plusPtr` offset) (fromIntegral len)
                    poke (ctext `plusPtr` len) (0 :: CChar)
                    return ctext
            Nothing -> return nullPtr

foreign import ccall unsafe "hexpatSetEntityHandler"
  _hexpatSetEntityHandler :: MyParserPtr -> FunPtr CEntityHandler -> IO ()

foreign import ccall "&free" funPtrFree :: FunPtr (Ptr Word8 -> IO ())

