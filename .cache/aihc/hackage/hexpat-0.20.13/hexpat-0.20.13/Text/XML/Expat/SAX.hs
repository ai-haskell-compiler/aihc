{-# LANGUAGE DeriveDataTypeable, TypeSynonymInstances, CPP, ScopedTypeVariables, FlexibleInstances, GADTs #-}
{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}

-- hexpat, a Haskell wrapper for expat
-- Copyright (C) 2008 Evan Martin <martine@danga.com>
-- Copyright (C) 2009 Stephen Blackheath <http://blacksapphire.com/antispam>

-- | This module provides functions to parse an XML document to a lazy
-- stream of SAX events.
module Text.XML.Expat.SAX (
  -- * XML primitives
  Encoding(..),
  XMLParseError(..),
  XMLParseLocation(..),

  -- * SAX-style parse
  ParseOptions(..),
  SAXEvent(..),

  parse,
  parseG,
  parseLocations,
  parseLocationsG,
  parseLocationsThrowing,
  parseThrowing,
  defaultParseOptions,

  -- * Variants that throw exceptions
  XMLParseException(..),

  -- * Abstraction of string types
  GenericXMLString(..)
  ) where

import Control.Concurrent.MVar
import Control.Exception as Exc
import Text.XML.Expat.Internal.IO
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Internal as I
import Data.Int
import Data.ByteString.Internal (c2w, w2c, c_strlen)
import qualified Data.Monoid as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Codec.Binary.UTF8.String as U8
import Data.List.Class (List(..), ListItem(..), cons, fromList, mapL)
import Data.Typeable
import Data.Word
import Control.Applicative
import Control.DeepSeq
import Control.Monad
import System.IO.Unsafe
import Foreign.C
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable


data ParseOptions tag text = ParseOptions
    { overrideEncoding :: Maybe Encoding
          -- ^ The encoding parameter, if provided, overrides the document's
          -- encoding declaration.
    , entityDecoder  :: Maybe (tag -> Maybe text)
          -- ^ If provided, entity references (i.e. @&nbsp;@ and friends) will
          -- be decoded into text using the supplied lookup function
    }

defaultParseOptions :: ParseOptions tag text
defaultParseOptions = ParseOptions Nothing Nothing


-- | An abstraction for any string type you want to use as xml text (that is,
-- attribute values or element text content). If you want to use a
-- new string type with /hexpat/, you must make it an instance of
-- 'GenericXMLString'.
class (M.Monoid s, Eq s) => GenericXMLString s where
    gxNullString :: s -> Bool
    gxToString :: s -> String
    gxFromString :: String -> s
    gxFromChar :: Char -> s
    gxHead :: s -> Char
    gxTail :: s -> s
    gxBreakOn :: Char -> s -> (s, s)
    gxFromByteString :: B.ByteString -> s
    gxToByteString :: s -> B.ByteString

instance GenericXMLString String where
    gxNullString = null
    gxToString = id
    gxFromString = id
    gxFromChar c = [c]
    gxHead = head
    gxTail = tail
    gxBreakOn c = break (==c)
    gxFromByteString = U8.decode . B.unpack
    gxToByteString = B.pack . map c2w . U8.encodeString

instance GenericXMLString B.ByteString where
    gxNullString = B.null
    gxToString = U8.decodeString . map w2c . B.unpack
    gxFromString = B.pack . map c2w . U8.encodeString
    gxFromChar = B.singleton . c2w
    gxHead = w2c . B.head
    gxTail = B.tail
    gxBreakOn c = B.break (== c2w c)
    gxFromByteString = id
    gxToByteString = id

instance GenericXMLString T.Text where
    gxNullString = T.null
    gxToString = T.unpack
    gxFromString = T.pack
    gxFromChar = T.singleton
    gxHead = T.head
    gxTail = T.tail
#if MIN_VERSION_text(0,11,0)
    gxBreakOn c = T.break (==c)
#elif MIN_VERSION_text(0,10,0)
    -- breakBy gets renamed to break between 0.10.0.0 and 0.10.0.1.
    -- There's no 'break' function that is consistent between these two
    -- versions so we work around it using other functions.
    gxBreakOn c t = (T.takeWhile (/=c) t, T.dropWhile (/=c) t)
#else
    gxBreakOn c = T.breakBy (==c)
#endif
    gxFromByteString = TE.decodeUtf8
    gxToByteString = TE.encodeUtf8

data SAXEvent tag text =
    XMLDeclaration text (Maybe text) (Maybe Bool) |
    StartElement tag [(tag, text)] |
    EndElement tag |
    CharacterData text |
    StartCData |
    EndCData |
    ProcessingInstruction text text |
    Comment text |
    FailDocument XMLParseError
    deriving (Eq, Show)

instance (NFData tag, NFData text) => NFData (SAXEvent tag text) where
    rnf (XMLDeclaration ver mEnc mSD) = rnf ver `seq` rnf mEnc `seq` rnf mSD
    rnf (StartElement tag atts) = rnf tag `seq` rnf atts
    rnf (EndElement tag) = rnf tag
    rnf (CharacterData text) = rnf text
    rnf StartCData = ()
    rnf EndCData = ()
    rnf (ProcessingInstruction target text) = rnf target `seq` rnf text
    rnf (Comment text) = rnf text
    rnf (FailDocument err) = rnf err

-- | Parse a generalized list of ByteStrings containing XML to SAX events.
-- In the event of an error, FailDocument is the last element of the output list.
parseG :: forall tag text l . (GenericXMLString tag, GenericXMLString text, List l) =>
          ParseOptions tag text -- ^ Parse options
       -> l ByteString          -- ^ Input text (a lazy ByteString)
       -> l (SAXEvent tag text)
{-# NOINLINE parseG #-}
parseG opts inputBlocks = mapL (return . fst) $ parseImpl opts inputBlocks False noExtra failureA
  where noExtra _ offset = return ((), offset)
        failureA _ = return ()

-- | Parse a generalized list of ByteStrings containing XML to SAX events.
-- In the event of an error, FailDocument is the last element of the output list.
parseLocationsG :: forall tag text l . (GenericXMLString tag, GenericXMLString text, List l) =>
                   ParseOptions tag text -- ^ Parse options
                -> l ByteString          -- ^ Input text (a lazy ByteString)
                -> l (SAXEvent tag text, XMLParseLocation)
{-# NOINLINE parseLocationsG #-}
parseLocationsG opts inputBlocks = parseImpl opts inputBlocks True fetchLocation id
  where
    fetchLocation pBuf offset = do
        [a, b, c, d] <- peekArray 4 (pBuf `plusPtr` offset :: Ptr Int64)
        return (XMLParseLocation a b c d, offset + 32)

parseImpl :: forall a tag text l . (GenericXMLString tag, GenericXMLString text, List l) =>
             ParseOptions tag text -- ^ Parse options
          -> l ByteString          -- ^ Input text (a lazy ByteString)
          -> Bool                  -- ^ True to add locations to binary output
          -> (Ptr Word8 -> Int -> IO (a, Int)) -- ^ Fetch extra data
          -> (IO XMLParseLocation -> IO a)     -- ^ Fetch a value for failure case 
          -> l (SAXEvent tag text, a)
parseImpl opts inputBlocks addLocations extra failureA = runParser inputBlocks parse cacheRef
  where
    (parse, getLocation, cacheRef) = unsafePerformIO $ do
        (parse, getLocation) <- hexpatNewParser
            (overrideEncoding opts)
            ((\decode -> fmap gxToByteString . decode . gxFromByteString) <$> entityDecoder opts)
            addLocations

        cacheRef <- newMVar Nothing
        return (parse, getLocation, cacheRef)

    runParser iblks parse cacheRef = joinL $ do
        li <- runList iblks
        return $ unsafePerformIO $ do
            mCached <- takeMVar cacheRef
            case mCached of
                Just l -> do
                    putMVar cacheRef mCached
                    return l
                Nothing -> do
                    (saxen, rema) <- case li of
                        Nil         -> do
                            (buf, len, mError) <- parse B.empty True
                            saxen <- parseBuf buf len extra
                            rema <- handleFailure mError mzero
                            return (saxen, rema)
                        Cons blk t -> {-unsafeInterleaveIO $-} do
                            (buf, len, mError) <- parse blk False
                            saxen <- parseBuf buf len extra
                            cacheRef' <- newMVar Nothing
                            rema <- handleFailure mError (runParser t parse cacheRef')
                            return (saxen, rema)
                    let l = fromList saxen `mplus` rema
                    putMVar cacheRef (Just l)
                    return l
      where
        handleFailure (Just err) _ = do a <- failureA getLocation
                                        return $ (FailDocument err, a) `cons` mzero
        handleFailure Nothing    l = return l

parseBuf :: (GenericXMLString tag, GenericXMLString text) =>
            ForeignPtr Word8 -> CInt -> (Ptr Word8 -> Int -> IO (a, Int)) -> IO [(SAXEvent tag text, a)]
parseBuf buf _ processExtra = withForeignPtr buf $ \pBuf -> doit [] pBuf 0
  where
    roundUp32 offset = (offset + 3) .&. complement 3
    doit acc pBuf offset0 = offset0 `seq` do
        typ <- peek (pBuf `plusPtr` offset0 :: Ptr Word32)
        (a, offset) <- processExtra pBuf (offset0 + 4)
        case typ of
            0 -> return (reverse acc)
            1 -> do
                nAtts <- peek (pBuf `plusPtr` offset :: Ptr Word32)
                let pName = pBuf `plusPtr` (offset + 4)
                lName <- fromIntegral <$> c_strlen pName
                let name = gxFromByteString $ I.fromForeignPtr buf (offset + 4) lName
                (atts, offset') <- foldM (\(atts, offset) _ -> do
                        let pAtt = pBuf `plusPtr` offset
                        lAtt <- fromIntegral <$> c_strlen pAtt
                        let att = gxFromByteString $ I.fromForeignPtr buf offset lAtt
                            offset' = offset + lAtt + 1
                            pValue = pBuf `plusPtr` offset'
                        lValue <- fromIntegral <$> c_strlen pValue
                        let value = gxFromByteString $ I.fromForeignPtr buf offset' lValue
                        return ((att, value):atts, offset' + lValue + 1)
                    ) ([], offset + 4 + lName + 1) [1,3..nAtts]
                doit ((StartElement name (reverse atts), a) : acc) pBuf (roundUp32 offset')
            2 -> do
                let pName = pBuf `plusPtr` offset
                lName <- fromIntegral <$> c_strlen pName
                let name = gxFromByteString $ I.fromForeignPtr buf offset lName
                    offset' = offset + lName + 1
                doit ((EndElement name, a) : acc) pBuf (roundUp32 offset')
            3 -> do
                len <- fromIntegral <$> peek (pBuf `plusPtr` offset :: Ptr Word32)
                let text = gxFromByteString $ I.fromForeignPtr buf (offset + 4) len
                    offset' = offset + 4 + len
                doit ((CharacterData text, a) : acc) pBuf (roundUp32 offset')
            4 -> do
                let pEnc = pBuf `plusPtr` offset
                lEnc <- fromIntegral <$> c_strlen pEnc
                let enc = gxFromByteString $ I.fromForeignPtr buf offset lEnc
                    offset' = offset + lEnc + 1
                    pVer = pBuf `plusPtr` offset'
                pVerFirst <- peek (castPtr pVer :: Ptr Word8)
                (mVer, offset'') <- case pVerFirst of
                    0 -> return (Nothing, offset' + 1)
                    1 -> do
                        lVer <- fromIntegral <$> c_strlen (pVer `plusPtr` 1)
                        return (Just $ gxFromByteString $ I.fromForeignPtr buf (offset' + 1) lVer, offset' + 1 + lVer + 1)
                    _ -> error "hexpat: bad data from C land"
                cSta <- peek (pBuf `plusPtr` offset'' :: Ptr Int8)
                let sta = if cSta < 0  then Nothing else
                          if cSta == 0 then Just False else
                                            Just True
                doit ((XMLDeclaration enc mVer sta, a) : acc) pBuf (roundUp32 (offset'' + 1))
            5 -> doit ((StartCData, a) : acc) pBuf offset
            6 -> doit ((EndCData, a) : acc) pBuf offset
            7 -> do
                let pTarget = pBuf `plusPtr` offset
                lTarget <- fromIntegral <$> c_strlen pTarget
                let target = gxFromByteString $ I.fromForeignPtr buf offset lTarget
                    offset' = offset + lTarget + 1
                    pData = pBuf `plusPtr` offset'
                lData <- fromIntegral <$> c_strlen pData
                let dat = gxFromByteString $ I.fromForeignPtr buf offset' lData
                doit ((ProcessingInstruction target dat, a) : acc) pBuf (roundUp32 (offset' + lData + 1))
            8 -> do
                let pText = pBuf `plusPtr` offset
                lText <- fromIntegral <$> c_strlen pText
                let text = gxFromByteString $ I.fromForeignPtr buf offset lText
                doit ((Comment text, a) : acc) pBuf (roundUp32 (offset + lText + 1))
            _ -> error "hexpat: bad data from C land"

-- | Lazily parse XML to SAX events. In the event of an error, FailDocument is
-- the last element of the output list.
parse :: (GenericXMLString tag, GenericXMLString text) =>
         ParseOptions tag text  -- ^ Parse options
      -> L.ByteString           -- ^ Input text (a lazy ByteString)
      -> [SAXEvent tag text]
parse opts input = parseG opts (L.toChunks input)


-- | An exception indicating an XML parse error, used by the /..Throwing/ variants.
data XMLParseException = XMLParseException XMLParseError
    deriving (Eq, Show, Typeable)

instance Exception XMLParseException where

-- | A variant of parseSAX that gives a document location with each SAX event.
parseLocations :: (GenericXMLString tag, GenericXMLString text) =>
                  ParseOptions tag text  -- ^ Parse options
               -> L.ByteString            -- ^ Input text (a lazy ByteString)
               -> [(SAXEvent tag text, XMLParseLocation)]
parseLocations opts input = parseLocationsG opts (L.toChunks input)


-- | Lazily parse XML to SAX events. In the event of an error, throw
-- 'XMLParseException'.
--
-- @parseThrowing@ can throw an exception from pure code, which is generally a bad
-- way to handle errors, because Haskell\'s lazy evaluation means it\'s hard to
-- predict where it will be thrown from.  However, it may be acceptable in
-- situations where it's not expected during normal operation, depending on the
-- design of your program.
parseThrowing :: (GenericXMLString tag, GenericXMLString text) =>
                 ParseOptions tag text  -- ^ Parse options
              -> L.ByteString            -- ^ input text (a lazy ByteString)
              -> [SAXEvent tag text]
parseThrowing opts bs = map freakOut $ parse opts bs
  where
    freakOut (FailDocument err) = Exc.throw $ XMLParseException err
    freakOut other = other


-- | A variant of parseSAX that gives a document location with each SAX event.
-- In the event of an error, throw 'XMLParseException'.
--
-- @parseLocationsThrowing@ can throw an exception from pure code, which is generally a bad
-- way to handle errors, because Haskell\'s lazy evaluation means it\'s hard to
-- predict where it will be thrown from.  However, it may be acceptable in
-- situations where it's not expected during normal operation, depending on the
-- design of your program.
parseLocationsThrowing :: (GenericXMLString tag, GenericXMLString text) =>
                          ParseOptions tag text  -- ^ Optional encoding override
                       -> L.ByteString            -- ^ Input text (a lazy ByteString)
                       -> [(SAXEvent tag text, XMLParseLocation)]
parseLocationsThrowing opts bs = map freakOut $ parseLocations opts bs
  where
    freakOut (FailDocument err, _) = Exc.throw $ XMLParseException err
    freakOut other = other
