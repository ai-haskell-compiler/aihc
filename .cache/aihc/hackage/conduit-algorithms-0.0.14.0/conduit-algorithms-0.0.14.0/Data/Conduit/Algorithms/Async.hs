{-|
Module      : Data.Conduit.Algorithms.Async
Copyright   : 2013-2022 Luis Pedro Coelho
License     : MIT
Maintainer  : luis@luispedro.org

Higher level async processing interfaces.
-}
{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, CPP, TupleSections, LambdaCase , BangPatterns #-}

module Data.Conduit.Algorithms.Async
    ( conduitPossiblyCompressedFile
    , conduitPossiblyCompressedToFile
    , withPossiblyCompressedFile
    , withPossiblyCompressedFileOutput
    , asyncMapC
    , asyncMapEitherC
    , asyncGzipTo
    , asyncGzipTo'
    , asyncGzipToFile
    , asyncGzipFrom
    , asyncGzipFromFile
    , asyncBzip2To
    , asyncBzip2ToFile
    , asyncBzip2From
    , asyncBzip2FromFile
    , asyncXzTo
    , asyncXzTo'
    , asyncXzToFile
    , asyncXzFrom
    , asyncXzFromFile
    , asyncZstdTo
    , asyncZstdToFile
    , asyncZstdFrom
    , asyncZstdFromFile
    , unorderedAsyncMapC
    ) where


import qualified Data.ByteString as B
import qualified Control.Concurrent.Async as A
import qualified Control.Concurrent.STM.TBQueue as TQ
import           Control.Concurrent.STM (atomically)

import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.Async as CA
import qualified Data.Conduit.TQueue as CA
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Zlib as CZ
import qualified Data.Conduit.Lzma2 as CX
import qualified Data.Streaming.Zlib as SZ
import qualified Data.Conduit.BZlib as CBZ
import qualified Data.Conduit.Zstd as CZstd
import qualified Control.Monad.Trans.Resource as R
import qualified Data.Conduit as C
import           Data.Conduit ((.|))

import qualified Data.Sequence as Seq
import           Data.Sequence ((|>), ViewL(..))
import           Data.Foldable (toList)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Error.Class (MonadError(..))
import           Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import           Control.Monad.Trans.Resource (MonadResource)
import           Control.Monad.Catch (MonadThrow)
import           Control.Exception (evaluate, displayException)
import           Control.DeepSeq (NFData, force)
import           System.IO.Error (mkIOError, userErrorType)
import           System.IO
import qualified System.IO as IO
import           Data.List (isSuffixOf)
import           Data.Conduit.Algorithms.Utils (awaitJust)



-- | This is like 'Data.Conduit.List.map', except that each element is processed
-- in a separate thread (up to 'maxThreads' can be queued up at any one time).
-- Results are evaluated to normal form (not weak-head normal form!, i.e., the
-- structure is deeply evaluated) to ensure that the computation is fully
-- evaluated in the worker thread.
--
-- Note that there is some overhead in threading. It is often a good idea to
-- build larger chunks of input before passing it to 'asyncMapC' to amortize
-- the costs. That is, when @f@ is not a lot of work, instead of @asyncMapC f@,
-- it is sometimes better to do
--
-- @
--    CC.conduitVector 4096 .| asyncMapC (V.map f) .| CC.concat
-- @
--
-- where @CC@ refers to 'Data.Conduit.Combinators'
--
-- See 'unorderedAsyncMapC'
asyncMapC :: forall a m b . (MonadIO m, NFData b) =>
                    Int -- ^ Maximum number of worker threads
                    -> (a -> b) -- ^ Function to execute
                    -> C.ConduitT a b m ()
asyncMapC = asyncMapCHelper True

-- | A version of 'asyncMapC' which can reorder results in the stream
--
-- If the order of the results is not important, this function can lead to a
-- better use of resources if some of the chunks take longer to complete.
--
-- See 'asyncMapC'
unorderedAsyncMapC :: forall a m b . (MonadIO m, NFData b) =>
                    Int -- ^ Maximum number of worker threads
                    -> (a -> b) -- ^ Function to execute
                    -> C.ConduitT a b m ()
unorderedAsyncMapC = asyncMapCHelper False

asyncMapCHelper  :: forall a m b . (MonadIO m, NFData b) =>
                    Bool
                    -> Int -- ^ Maximum number of worker threads
                    -> (a -> b) -- ^ Function to execute
                    -> C.ConduitT a b m ()
asyncMapCHelper isSynchronous maxThreads f = initLoop (0 :: Int) (Seq.empty :: Seq.Seq (A.Async b))
    where
        initLoop :: Int -> Seq.Seq (A.Async b) -> C.ConduitT a b m ()
        initLoop size q
            | size == maxThreads = loop q
            | otherwise = C.await >>= \case
                Nothing -> yAll q
                Just v -> do
                        v' <- sched v
                        initLoop (size + 1) (q |> v')
        sched :: a -> C.ConduitM a b m (A.Async b)
        sched v = liftIO . A.async . evaluate . force $ f v

        -- | yield all
        yAll :: Seq.Seq (A.Async b) -> C.ConduitT a b m ()
        yAll Seq.Empty = return ()
        yAll q = do
            (r, q') <- liftIO $ retrieveResult q
            C.yield r
            yAll q'

        loop :: Seq.Seq (A.Async b) -> C.ConduitT a b m ()
        loop q = C.await >>= \case
                Nothing -> yAll q
                Just v -> do
                    v' <- sched v
                    (r, q') <- liftIO $ retrieveResult q
                    C.yield r
                    loop (q' |> v')

        retrieveResult :: Seq.Seq (A.Async b) -> IO (b, Seq.Seq (A.Async b))
        retrieveResult q
            | isSynchronous = case Seq.viewl q of
                        (r :< rest) -> (, rest) <$> A.wait r
                        _ -> error "Impossible situation"
            | otherwise = do
                (k, r) <- liftIO (A.waitAny (toList q))
                return (r, Seq.filter (/= k) q)


-- | 'asyncMapC' with error handling. The inner function can now return an
-- error (as a 'Left'). When the first error is seen, it 'throwError's in the
-- main monad. Note that 'f' may be evaluated for arguments beyond the first
-- error (as some threads may be running in the background and already
-- processing elements after the first error).
--
-- See 'asyncMapC'
asyncMapEitherC :: forall a m b e . (MonadIO m, NFData b, NFData e, MonadError e m) => Int -> (a -> Either e b) -> C.ConduitT a b m ()
asyncMapEitherC maxThreads f = asyncMapC maxThreads f .| (C.awaitForever $ \case
                                Right v -> C.yield v
                                Left err -> throwError err)


-- | concatenates input into larger chunks and yields it. Its indended use is
-- to build up larger blocks from smaller ones so that they can be sent across
-- thread barriers with little overhead.
--
-- the chunkSize parameter is a hint, not an exact element. In particular,
-- larger chunks are not split up and smaller chunks can be yielded too.
bsConcatTo :: MonadIO m => Int -- ^ chunk hint
                            -> C.ConduitT B.ByteString [B.ByteString] m ()
bsConcatTo chunkSize = C.awaitForever (\v -> continue [v] (B.length v))
    where
        continue chunks !s
            | s >= chunkSize = C.yield chunks
            | otherwise = C.await >>= maybe
                                        (C.yield chunks)
                                        (\v -> continue (v:chunks) (s + B.length v))

untilNothing :: forall m i. (Monad m) => C.ConduitT (Maybe i) i m ()
untilNothing = awaitJust $ \case
    Just val -> do
        C.yield val
        untilNothing
    _ -> return ()


genericAsyncFrom :: forall m. (MonadIO m, MonadUnliftIO m) => C.ConduitT B.ByteString B.ByteString m () -> Handle -> C.ConduitT () B.ByteString m ()
genericAsyncFrom transform h = do
    let prod q = do
                    C.runConduit $
                        C.sourceHandle h
                            .| transform
                            .| CL.map Just
                            .| CA.sinkTBQueue q
                    liftIO $ atomically (TQ.writeTBQueue q Nothing)
    CA.gatherFrom 8 prod .| untilNothing

genericAsyncTo :: forall m. (MonadIO m, MonadUnliftIO m) => C.ConduitT B.ByteString B.ByteString (R.ResourceT IO) () -> Handle -> C.ConduitT B.ByteString C.Void m ()
genericAsyncTo tranform h = do
    let drain q = liftIO . C.runConduitRes $
                CA.sourceTBQueue q
                    .| untilNothing
                    .| CL.map (B.concat . reverse)
                    .| tranform
                    .| C.sinkHandle h
    bsConcatTo ((2 :: Int) ^ (15 :: Int))
        .| CA.drainTo 8 drain


genericFromFile :: forall m. (MonadResource m, MonadUnliftIO m) => (Handle -> C.ConduitT () B.ByteString m ()) -> FilePath -> C.ConduitT () B.ByteString m ()
genericFromFile from fname = C.bracketP
    (openFile fname ReadMode)
    hClose
    from

genericToFile :: forall m. (MonadResource m, MonadUnliftIO m) => (Handle -> C.ConduitT B.ByteString C.Void m ()) -> FilePath -> C.ConduitT B.ByteString C.Void m ()
genericToFile to fname = C.bracketP
    (openFile fname WriteMode)
    hClose
    to

-- | A simple sink which performs gzip compression in a separate thread and
-- writes the results to `h`.
--
-- See also 'asyncGzipToFile'
asyncGzipTo :: forall m. (MonadIO m, MonadUnliftIO m) => Handle -> C.ConduitT B.ByteString C.Void m ()
asyncGzipTo = asyncGzipTo' (-1)

-- | A simple sink which performs gzip compression in a separate thread and
-- writes the results to `h` with a given compression level.
asyncGzipTo' :: forall m. (MonadIO m, MonadUnliftIO m) => Int -> Handle -> C.ConduitT B.ByteString C.Void m ()
asyncGzipTo' clevel h = genericAsyncTo gz h
    where
        gz = CZ.compress clevel (CZ.WindowBits 31) `C.catchC` handleZLibException
        handleZLibException = \(e :: SZ.ZlibException) ->
                                    liftIO . ioError $ mkIOError userErrorType ("Error compressing gzip stream: "++displayException e) (Just h) Nothing

-- | Compresses the output and writes to the given file with compression being
-- performed in a separate thread.
--
-- See also 'asyncGzipTo'
asyncGzipToFile :: forall m. (MonadResource m, MonadUnliftIO m) => FilePath -> C.ConduitT B.ByteString C.Void m ()
asyncGzipToFile = genericToFile asyncGzipTo

-- | A source which produces the ungzipped content from the the given handle.
-- Note that this "reads ahead" so if you do not use all the input, the Handle
-- will probably be left at an undefined position in the file.
--
-- Note: unlike the ungzip conduit from 'Data.Conduit.Zlib', this function will
-- read *all* the compressed files in the stream (not just the first).
--
-- See also 'asyncGzipFromFile'
asyncGzipFrom :: forall m. (MonadIO m, MonadResource m, MonadUnliftIO m) => Handle -> C.ConduitT () B.ByteString m ()
asyncGzipFrom h = do
    let prod q = liftIO $ do
                    C.runConduit $
                        C.sourceHandle h
                            .| CZ.multiple CZ.ungzip
                            .| CL.map Just
                            .| CA.sinkTBQueue q
                    atomically (TQ.writeTBQueue q Nothing)
    (CA.gatherFrom 8 prod .| untilNothing)
        `C.catchC`
        (\(e :: SZ.ZlibException) -> liftIO . ioError $ mkIOError userErrorType ("Error reading gzip file: "++displayException e) (Just h) Nothing)

-- | Open and read a gzip file with the uncompression being performed in a
-- separate thread.
--
-- See also 'asyncGzipFrom'
asyncGzipFromFile :: forall m. (MonadResource m, MonadUnliftIO m) => FilePath -> C.ConduitT () B.ByteString m ()
asyncGzipFromFile = genericFromFile asyncGzipFrom

-- | A simple sink which performs bzip2 compression in a separate thread and
-- writes the results to `h`.
--
-- See also 'asyncBzip2ToFile'
asyncBzip2To :: forall m. (MonadIO m, MonadUnliftIO m) => Handle -> C.ConduitT B.ByteString C.Void m ()
asyncBzip2To = genericAsyncTo CBZ.bzip2

-- | Compresses the output and writes to the given file with compression being
-- performed in a separate thread.
--
-- See also 'asyncBzip2To'
asyncBzip2ToFile :: forall m. (MonadResource m, MonadUnliftIO m) => FilePath -> C.ConduitT B.ByteString C.Void m ()
asyncBzip2ToFile = genericToFile asyncBzip2To

-- | A source which produces the bzipped2 content from the the given handle.
-- Note that this "reads ahead" so if you do not use all the input, the Handle
-- will probably be left at an undefined position in the file.
--
-- See also 'asyncBzip2FromFile'
asyncBzip2From :: forall m. (MonadIO m, MonadResource m, MonadUnliftIO m) => Handle -> C.ConduitT () B.ByteString m ()
asyncBzip2From = genericAsyncFrom (CZ.multiple CBZ.bunzip2)

-- | Open and read a bzip2 file with the uncompression being performed in a
-- separate thread.
--
-- See also 'asyncBzip2From'
asyncBzip2FromFile :: forall m. (MonadResource m, MonadUnliftIO m) => FilePath -> C.ConduitT () B.ByteString m ()
asyncBzip2FromFile = genericFromFile asyncBzip2From

-- | A simple sink which performs lzma/xz compression in a separate thread and
-- writes the results to `h`.
--
-- See also 'asyncXzToFile'
asyncXzTo :: forall m. (MonadIO m, MonadResource m, MonadUnliftIO m) => Handle -> C.ConduitT B.ByteString C.Void m ()
asyncXzTo = genericAsyncTo (CX.compress Nothing)

-- | A simple sink which performs lzma/xz compression in a separate thread and
-- writes the results to `h`.
--
-- See also 'asyncXzToFile' and 'asyncXzTo'
asyncXzTo' :: forall m. (MonadIO m, MonadResource m, MonadUnliftIO m) => Int -> Handle -> C.ConduitT B.ByteString C.Void m ()
asyncXzTo' clevel = genericAsyncTo (CX.compress (Just clevel))

-- | Compresses the output and writes to the given file with compression being
-- performed in a separate thread.
--
-- See also 'asyncXzTo'
asyncXzToFile :: forall m. (MonadResource m, MonadUnliftIO m) => FilePath -> C.ConduitT B.ByteString C.Void m ()
asyncXzToFile = genericToFile asyncXzTo

-- | A source which produces the unxzipped content from the the given handle.
-- Note that this "reads ahead" so if you do not use all the input, the Handle
-- will probably be left at an undefined position in the file.
--
-- See also 'asyncXzFromFile'
asyncXzFrom :: forall m. (MonadIO m, MonadResource m, MonadUnliftIO m, MonadThrow m) => Handle -> C.ConduitT () B.ByteString m ()
asyncXzFrom =
    let oneGBmembuffer = Just $ 1024 ^ (3 :: Integer)
    in genericAsyncFrom (CX.decompress oneGBmembuffer)

-- | Open and read a lzma/xz file with the uncompression being performed in a
-- separate thread.
--
-- See also 'asyncXzFrom'
asyncXzFromFile :: forall m. (MonadResource m, MonadUnliftIO m, MonadThrow m) => FilePath -> C.ConduitT () B.ByteString m ()
asyncXzFromFile = genericFromFile asyncXzFrom

-- | Decompress ZStd format using a separate thread
--
-- See also 'asyncZstdFromFile'
asyncZstdFrom :: forall m. (MonadIO m, MonadUnliftIO m) => Handle -> C.ConduitT () B.ByteString m ()
asyncZstdFrom = genericAsyncFrom CZstd.decompress

-- | Compress in ZStd format using a separate thread and write to a file
-- See also 'asyncZstdFrom'
asyncZstdFromFile :: forall m. (MonadResource m, MonadUnliftIO m, MonadThrow m) => FilePath -> C.ConduitT () B.ByteString m ()
asyncZstdFromFile = genericFromFile asyncZstdFrom


-- | Compress in Zstd format using a separate thread
-- 
-- See also 'asyncZstdToFile'
asyncZstdTo :: forall m. (MonadIO m, MonadUnliftIO m) =>
                Int -- ^ compression level
                -> Handle -> C.ConduitT B.ByteString C.Void m ()
asyncZstdTo clevel = genericAsyncTo (CZstd.compress clevel)


-- | Compress in ZStd format using a separate thread and write to a file
--
-- This will use compression level 3 as this is the default in the ZStd C API
--
-- See also 'asyncZstdTo'
asyncZstdToFile :: forall m. (MonadResource m, MonadUnliftIO m) => FilePath -> C.ConduitT B.ByteString C.Void m ()
asyncZstdToFile = genericToFile (asyncZstdTo 3)

-- | If the filename indicates a supported compressed file (gzip, xz, and, on
-- Unix, bzip2), then it reads it and uncompresses it.
--
-- Usage
--
-- @
--
--      withPossiblyCompressedFile fname $ \src ->
--          runConduit (src .| mySink)
-- @
--
-- Unlike 'conduitPossiblyCompressedFile', this ensures that the file is closed
-- even if the conduit terminates early.
--
-- On Windows, attempting to read from a bzip2 file, results in 'error'.
withPossiblyCompressedFile :: (MonadUnliftIO m, MonadResource m, MonadThrow m) => FilePath -> (C.ConduitT () B.ByteString m () -> m a) -> m a
withPossiblyCompressedFile fname inner = withRunInIO $ \run -> do
    IO.withBinaryFile fname IO.ReadMode $
        run . inner . withPossiblyCompressedFile' fname

withPossiblyCompressedFile' :: (MonadUnliftIO m, MonadResource m, MonadThrow m) => FilePath -> Handle -> C.ConduitT () B.ByteString m ()
withPossiblyCompressedFile' fname
    | ".gz" `isSuffixOf` fname = asyncGzipFrom
    | ".xz" `isSuffixOf` fname = asyncXzFrom
    | ".bz2" `isSuffixOf` fname = asyncBzip2From
    | ".zst" `isSuffixOf` fname = asyncZstdFrom
    | ".zstd" `isSuffixOf` fname = asyncZstdFrom
    | otherwise = C.sourceHandle


-- | If the filename indicates a supported compressed file (gzip, xz, and, on
-- Unix, bzip2), then it provides an output source
--
-- Usage
--
-- @
--
--      withPossiblyCompressedFileOutput fname $ \out ->
--          runConduit (mySrc .| out)
-- @
--
-- This ensures that the file is closed even if the conduit terminates early.
--
-- On Windows, attempting to read from a bzip2 file, results in 'error'.
withPossiblyCompressedFileOutput :: (MonadUnliftIO m, MonadResource m, MonadThrow m) => FilePath -> (C.ConduitT B.ByteString C.Void m () -> m a) -> m a
withPossiblyCompressedFileOutput fname inner = withRunInIO $ \run -> do
    IO.withBinaryFile fname IO.WriteMode $
        run . inner . withPossiblyCompressedFileOutput' fname


withPossiblyCompressedFileOutput' :: (MonadUnliftIO m, MonadResource m, MonadThrow m) => FilePath -> Handle -> C.ConduitT B.ByteString C.Void m ()
withPossiblyCompressedFileOutput' fname
    | ".gz" `isSuffixOf` fname = asyncGzipTo
    | ".xz" `isSuffixOf` fname = asyncXzTo' 6
    | ".bz2" `isSuffixOf` fname = asyncBzip2To
    | ".zst" `isSuffixOf` fname = asyncZstdTo 3
    | ".zstd" `isSuffixOf` fname = asyncZstdTo 3
    | otherwise = C.sinkHandle


-- | If the filename indicates a gzipped file (or, on Unix, also a bz2 file),
-- then it reads it and uncompresses it.
--
--
-- To ensure that the file is closed even if the downstream finishes early,
-- consider using 'withPossiblyCompressedFile'.
--
-- On Windows, attempting to read from a bzip2 file, results in 'error'.
conduitPossiblyCompressedFile :: (MonadUnliftIO m, MonadResource m, MonadThrow m) => FilePath -> C.ConduitT () B.ByteString m ()
conduitPossiblyCompressedFile fname
    | ".gz" `isSuffixOf` fname = asyncGzipFromFile fname
    | ".xz" `isSuffixOf` fname = asyncXzFromFile fname
    | ".bz2" `isSuffixOf` fname = asyncBzip2FromFile fname
    | ".zst" `isSuffixOf` fname = asyncZstdFromFile fname
    | ".zstd" `isSuffixOf` fname = asyncZstdFromFile fname
    | otherwise = C.sourceFile fname

-- | If the filename indicates a gzipped file (or, on Unix, also a bz2 file),
-- then it compresses and write with the algorithm matching the filename
--
-- Consider using 'withPossiblyCompressedFileOutput' to ensure prompt file closing.
--
-- On Windows, attempting to write to a bzip2 file, results in 'error'.
conduitPossiblyCompressedToFile :: (MonadUnliftIO m, MonadResource m) => FilePath -> C.ConduitT B.ByteString C.Void m ()
conduitPossiblyCompressedToFile fname
    | ".gz" `isSuffixOf` fname = asyncGzipToFile fname
    | ".xz" `isSuffixOf` fname = asyncXzToFile fname
    | ".bz2" `isSuffixOf` fname = asyncBzip2ToFile fname
    | ".zst" `isSuffixOf` fname = asyncZstdToFile fname
    | ".zstd" `isSuffixOf` fname = asyncZstdToFile fname
    | otherwise = C.sinkFile fname
