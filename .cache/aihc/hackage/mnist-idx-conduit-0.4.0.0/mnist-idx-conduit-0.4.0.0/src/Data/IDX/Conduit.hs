{-# LANGUAGE DeriveFoldable #-}
{-# language ScopedTypeVariables #-}
{-# options_ghc -Wno-unused-imports #-}
{-# options_ghc -Wno-unused-matches #-}
{-|

Streaming (de)serialization and encode-decode functions for the IDX format used in the MNIST handwritten digit recognition dataset [1].

Both sparse and dense decoders are provided. In either case, the range of the data is the same as the raw data (one unsigned byte per pixel).


== Links

1) http://yann.lecun.com/exdb/mnist/

-}
module Data.IDX.Conduit (
  -- * Source
  -- ** Labels
  sourceIdxLabels,
  mnistLabels,
  -- ** Data
  -- *** Dense
  sourceIdx,
  -- *** Sparse
  sourceIdxSparse,
  -- * Sink
  -- ** Data
  -- *** Dense
  sinkIdx,
  -- *** Sparse
  sinkIdxSparse,
  -- * Types
  Sparse,
  sBufSize, sNzComponents,
  -- * Debug
  readHeader
                        )where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Either (isRight)
import Data.Foldable (Foldable(..), traverse_, for_)
import Data.Int (Int8, Int16, Int32)
import Data.Word (Word8, Word16, Word32)
import Data.Void (Void)
import GHC.IO.Handle (Handle, hSeek, SeekMode(..), hClose)
import System.IO (IOMode(..), withBinaryFile, openBinaryFile)
-- binary
import Data.Binary (Binary(..), Get, getWord8, putWord8, encode, decode, decodeOrFail)
import Data.Binary.Get (runGetOrFail)
-- bytestring
import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Lazy as LBS (ByteString, hGet, readFile, toStrict, map)
import qualified Data.ByteString.Lazy.Internal as LBS (unpackBytes, packBytes)
-- conduit
import Conduit (MonadResource, runResourceT, (.|), runConduitRes)
import qualified Data.Conduit as C (ConduitT, runConduit, bracketP, yield)
import qualified Data.Conduit.Combinators as C (sinkFile, map, takeExactly, print, takeExactlyE)
-- containers
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as SQ (fromList)
-- vector
import qualified Data.Vector as V (Vector, replicateM, length, forM_, head, tail)
import qualified Data.Vector.Unboxed as VU (Unbox, Vector, length, fromList, toList, foldl, (!))


-- | Outputs dense data buffers in the 0-255 range
--
-- In the case of MNIST dataset, 0 corresponds to the background of the image.
sourceIdx :: MonadResource m =>
             FilePath -- ^ filepath of uncompressed IDX data file
          -> Maybe Int -- ^ optional maximum number of entries to retrieve
          -> C.ConduitT () (VU.Vector Word8) m ()
sourceIdx = sourceIDX_ (\ _ bs -> VU.fromList $ components bs)


-- | Outputs sparse data buffers (i.e without zero components)
--
-- This incurs at least one additional data copy of each vector, but the resulting vectors take up less space.
sourceIdxSparse :: MonadResource m =>
                   FilePath -- ^ filepath of uncompressed IDX data file
                -> Maybe Int -- ^ optional maximum number of entries to retrieve
                -> C.ConduitT () (Sparse Word8) m ()
sourceIdxSparse = sourceIDX_ (\n bs -> Sparse n (sparsify $ components bs))

-- | Parser for the labels, can be plugged in as an argument to 'sourceIdxLabels'
mnistLabels :: LBS.ByteString
            -> Either String Int
mnistLabels l
  | length xs == 1 = Right (head xs)
  | otherwise = Left "MNIST labels are the 0-9 digits"
  where xs = fromEnum `map` (LBS.unpackBytes l)

-- | Outputs the labels corresponding to the data
sourceIdxLabels :: MonadResource m =>
                   (LBS.ByteString -> Either e o) -- ^ parser for the labels, where the bytestring buffer contains exactly one unsigned byte
                -> FilePath -- ^ filepath of uncompressed IDX labels file
                -> Maybe Int -- ^ optional maximum number of entries to retrieve
                -> C.ConduitT () (Either e o) m r
sourceIdxLabels buildf fp mmax = withReadHdl fp $ \handle -> do
  hlbs <- liftIO $ LBS.hGet handle 4
  case decodeE hlbs of
    Left e -> error e
    Right magic@IDXMagic{} -> do
      nitbs <- liftIO $ LBS.hGet handle 4 -- number of items is 32 bit (4 byte)
      case decodeE nitbs of
        Left e -> error e
        Right (ndata :: Int) -> do
          let bufsize = 1
              go i = do
                let n = case mmax of
                      Nothing -> n
                      Just mi -> mi
                when (i < min n ndata) $ do
                  b <- liftIO $ LBS.hGet handle bufsize
                  liftIO $ hSeek handle RelativeSeek (fromIntegral bufsize)
                  C.yield $ buildf b
                go (succ i)
          go 0

decodeE :: Binary b => LBS.ByteString -> Either String b
decodeE l = case decodeOrFail l of
    Left (_, _, e) -> Left e
    Right (_, _, x) -> Right x


{-# WARNING sinkIdx "this produces an incomplete header for some reason, causing the decoder to chop the data items at the wrong length. Do not use until https://github.com/ocramz/mnist-idx-conduit/issues/1 is resolved." #-}
-- | Write a dataset to disk
--
-- Contents are written as unsigned bytes, so make sure 8 bit data comes in without losses
sinkIdx :: (MonadResource m, Foldable t) =>
           FilePath -- ^ file to write
        -> Int -- ^ number of data items that will be written
        -> t Word32 -- ^ data dimension sizes
        -> C.ConduitT (VU.Vector Word8) Void m ()
sinkIdx = sinkIDX_ (LBS.toStrict . fromComponents . VU.toList)

{-# WARNING sinkIdxSparse "this produces an incomplete header for some reason, causing the decoder to chop the data items at the wrong length. Do not use until https://github.com/ocramz/mnist-idx-conduit/issues/1 is resolved." #-}
-- | Write a sparse dataset to disk
--
-- Contents are written as unsigned bytes, so make sure 8 bit data comes in without losses
sinkIdxSparse :: (Foldable t, MonadResource m) =>
                 FilePath -- ^ file to write
              -> Int -- ^ number of data items that will be written
              -> t Word32 -- ^ data dimension sizes
              -> C.ConduitT (Sparse Word8) Void m ()
sinkIdxSparse = sinkIDX_ (\(Sparse n vu) -> LBS.toStrict $ fromComponents $ densify n vu)

{-# WARNING sinkIDX_ "this produces an incomplete header for some reason, causing the decoder to chop the data items at the wrong length. Do not use until https://github.com/ocramz/mnist-idx-conduit/issues/1 is resolved." #-}
sinkIDX_ :: (MonadResource m, Foldable t) =>
            (i -> BS.ByteString)
         -> FilePath
         -> Int -- ^ number of data items that will be written
         -> t Word32 -- ^ data dimension sizes
         -> C.ConduitT i Void m ()
sinkIDX_ buildf fp ndata ds = src .|
                              C.sinkFile fp
  where
    ndims = length ds
    magicbs = encodeBS (IDXMagic IDXUnsignedByte ndims)
    ndatabs = encodeBS (fromIntegral ndata :: Word32)
    src = do
      C.yield magicbs -- magic number
      C.yield ndatabs -- number of data items
      for_ ds $ \d -> do -- data dimension sizes
        let
          d32 :: Word32
          d32 = fromIntegral d
        C.yield (encodeBS d32)
      C.takeExactly ndata $ C.map buildf


encodeBS :: (Binary b) => b -> BS.ByteString
encodeBS = LBS.toStrict . encode

sourceIDX_ :: MonadResource m =>
              (Int -> LBS.ByteString -> o)
           -> FilePath -- ^ filepath of uncompressed IDX data file
           -> Maybe Int -- ^ optional maximum number of entries to retrieve
           -> C.ConduitT i o m ()
sourceIDX_ buildf fp mmax = withReadHdl fp $ \handle -> do
  hlbs <- liftIO $ LBS.hGet handle 4
  case decodeOrFail hlbs of
    Left (_, _, e) -> error e
    Right (_, _, IDXMagic _ ndims) -> do
      let
        bytesDimsVec = 4 * ndims -- each dim is a 32 bit (4 byte) int
      dvlbs <- liftIO $ LBS.hGet handle bytesDimsVec
      case getDims ndims dvlbs of
        Left e -> error e
        Right vv -> do
          let
            ndata = V.head vv
            bufsize = product $ V.tail vv
            go i = do
              let n = case mmax of
                    Nothing -> ndata
                    Just m  -> m
              when (i < min n ndata) $ do
                b <- liftIO $ LBS.hGet handle bufsize
                liftIO $ hSeek handle RelativeSeek (fromIntegral bufsize)
                C.yield $ buildf bufsize b
                go (succ i)
          go 0

sparsify :: (Foldable t) => t Word8 -> VU.Vector (Int, Word8)
sparsify xs = VU.fromList $ toList $ snd $ foldl ins (0, mempty) xs
  where
    ins (i, acc) x =
      let x' = fromEnum x
      in if x' /= 0
      then (succ i, acc |> (i, x))
      else (succ i, acc)

densify :: Int -> VU.Vector (Int, Word8) -> [Word8]
densify n vu = toList $ snd $ foldl ins (0, mempty) [0 .. n - 1]
  where
    nnz = VU.length vu
    ins (inz, acc) i
      | inz < nnz =
        let (iv, x) = vu VU.! inz
        in case i `compare` iv of
          EQ -> (succ inz, acc |> x)
          _ -> (inz, acc |> 0)
      | otherwise = (inz, acc |> 0)


components :: LBS.ByteString -> [Word8]
components = LBS.unpackBytes

fromComponents :: [Word8] -> LBS.ByteString
fromComponents = LBS.packBytes

-- | Sparse buffer (containing only nonzero entries)
data Sparse a = Sparse {
  sBufSize :: !Int -- ^ total number of entries in the _dense_ buffer, i.e. including the zeros
  , sNzComponents :: VU.Vector (Int, a) -- ^ nonzero components, together with the linear index into their dense counterpart
  }  deriving (Eq, Show)

getDims :: Num a =>
           Int -- ^ number of dimensions
        -> LBS.ByteString -> Either String (V.Vector a)
getDims n lbs = case runGetOrFail gg lbs of
  Left (_, _, e) -> Left e
  Right (_, _, x) -> Right x
  where
    gg = V.replicateM n (fromIntegral <$> getInt32)

withReadHdl :: MonadResource m =>
               FilePath
            -> (Handle -> C.ConduitT i o m r) -- ^ read from the handle
            -> C.ConduitT i o m r
withReadHdl fp = C.bracketP (openBinaryFile fp ReadMode) hClose

withReadHdl_ :: FilePath -> (Handle -> IO r) -> IO r
withReadHdl_ fp = withBinaryFile fp ReadMode

-- | Decode the header of an IDX data file and print out its contents
readHeader :: FilePath -- ^ path of IDX file
           -> IO (IDXMagic, Int32, V.Vector Int32) -- ^ "magic number", number of data items, list of dimension sizes of each data item
readHeader fp = withReadHdl_ fp $ \handle -> do
  hlbs <- liftIO $ LBS.hGet handle 4
  case decodeOrFail hlbs of
    Left (_, _, e) -> error e
    Right (_, _, mg@(IDXMagic _ ndims)) -> do
      let
        bytesDimsVec = 4 * ndims -- each dim is a 32 bit (4 byte) int
      dvlbs <- liftIO $ LBS.hGet handle bytesDimsVec
      case getDims ndims dvlbs of
        Left e -> error e
        Right vv -> do
          let
            ndata = V.head vv
            bufsizes = V.tail vv
          pure (mg, ndata, bufsizes)


-- | "magic number" starting the file header for the IDX format
--
-- as per http://yann.lecun.com/exdb/mnist/
--
-- 32 bit (4 bytes) header ("magic number")
data IDXMagic = IDXMagic {
  idxType :: IDXContentType
  , idxNumDims :: Int
                         } deriving (Show)

instance Binary IDXMagic where
  get = do
    -- first 2 bytes are 0
    _ <- getWord8 >> getWord8
    -- third byte encodes the type of data
    ty <- get :: Get IDXContentType
    -- fourth byte encode the number of dimensions
    nDims <- fromIntegral <$> getWord8
    pure $ IDXMagic ty nDims
  put d = do
    -- first 2 bytes are 0
    putWord8 0 >> putWord8 0
    -- third byte encodes the type of data
    put $ idxType d
    -- fourth byte encode the number of dimensions
    put $ (fromIntegral (idxNumDims d) :: Word8)


-- | A type to describe the content, according to IDX spec
data IDXContentType =
   IDXUnsignedByte
   | IDXSignedByte
   | IDXShort
   | IDXInt
   | IDXFloat
   | IDXDouble
   deriving Show

instance Binary IDXContentType where
    get = do
      w <- getWord8
      case w of
        0x08 -> return IDXUnsignedByte
        0x09 -> return IDXSignedByte
        0x0B -> return IDXShort
        0x0C -> return IDXInt
        0x0D -> return IDXFloat
        0x0E -> return IDXDouble
        _ -> fail $ "Unrecognized IDX content type: " ++ (show w)

    put IDXUnsignedByte = putWord8 0x08
    put IDXSignedByte   = putWord8 0x09
    put IDXShort        = putWord8 0x0B
    put IDXInt          = putWord8 0x0C
    put IDXFloat        = putWord8 0x0D
    put IDXDouble       = putWord8 0x0E

-- Data.Binary uses big-endian format
-- getInt8 :: Get Int8
-- getInt8 = get

-- getInt16 :: Get Int16
-- getInt16 = get

getInt32 :: Get Int32
getInt32 = get

-- getFloat :: Get Float
-- getFloat = get

-- getDouble :: Get Double
-- getDouble = get
