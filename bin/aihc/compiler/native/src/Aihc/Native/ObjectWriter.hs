-- | Store section bytes with a bounded buffer for each section.
module Aihc.Native.ObjectWriter
  ( ObjectWriter,
    withObjectWriter,
    modifyObject,
    alignObject,
  )
where

import Aihc.Native.Object
import Control.Exception (bracket, mask_, onException)
import Control.Monad (forM, forM_, unless, when)
import Data.Bits (shiftL)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map.Strict qualified as Map
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile, renameFile)
import System.FilePath (takeDirectory)
import System.IO (Handle, SeekMode (AbsoluteSeek), hClose, hFlush, hSeek, openBinaryTempFile)

data ObjectWriter = ObjectWriter
  { writerDraft :: !(IORef Draft),
    writerSections :: !(IORef (Map.Map SectionRole (FilePath, Handle))),
    writerDirectory :: !FilePath
  }

-- | Publish the object only after all compiler and object checks succeed.
withObjectWriter :: FilePath -> (Image -> Either ObjectError BL.ByteString) -> (ObjectWriter -> IO value) -> IO value
withObjectWriter destination encode action = do
  createDirectoryIfMissing True directory
  bracket acquire release $ \writer -> do
    value <- action writer
    draft <- readIORef (writerDraft writer)
    files <- readIORef (writerSections writer)
    payloads <- fmap Map.fromList $ forM (Map.toList (draftSections draft)) $ \(role, section) -> do
      prefix <- case Map.lookup role files of
        Nothing -> pure BL.empty
        Just (_, handle) -> do
          hFlush handle
          hSeek handle AbsoluteSeek 0
          BL.hGetContents handle
      pure (role, prefix <> sectionBytes section)
    bytes <- checked (layoutDraftWith (\role _ -> payloads Map.! role) draft >>= encode)
    bracket (openBinaryTempFile directory ".aihc-object") (\(path, handle) -> hClose handle >> removeIfPresent path) $ \(path, handle) -> do
      BL.hPut handle bytes
      hClose handle
      renameFile path destination
    pure value
  where
    directory = takeDirectory destination
    acquire = ObjectWriter <$> newIORef emptyDraft <*> newIORef Map.empty <*> pure directory
    release writer = do
      files <- readIORef (writerSections writer)
      forM_ (Map.elems files) $ \(path, handle) -> hClose handle >> removeFile path

-- | Append bytes and metadata, then release each completed byte chunk.
modifyObject :: ObjectWriter -> (Draft -> Either ObjectError Draft) -> IO ()
modifyObject writer update = do
  draft <- readIORef (writerDraft writer) >>= checked . update
  let section = draftCurrent draft
      chunks = sectionChunksRev section
  unless (null chunks) $ case draftCurrentSection draft of
    Nothing -> ioError (userError "Object bytes have no section.")
    Just role -> do
      files <- readIORef (writerSections writer)
      handle <- case Map.lookup role files of
        Just (_, existing) -> pure existing
        Nothing -> mask_ $ do
          file <- openBinaryTempFile (writerDirectory writer) ".aihc-section"
          writeIORef (writerSections writer) (Map.insert role file files) `onException` (hClose (snd file) >> removeFile (fst file))
          pure (snd file)
      mapM_ (BS.hPut handle) (reverse chunks)
  writeIORef (writerDraft writer) draft {draftCurrent = section {sectionChunksRev = []}}

-- | Emit alignment fill in bounded chunks, including large data alignments.
alignObject :: ObjectWriter -> Int -> (Draft -> BS.ByteString) -> IO ()
alignObject writer power fillFor = do
  draft <- readIORef (writerDraft writer)
  let fill = fillFor draft
  checked $ case draftCurrentSection draft of
    Nothing -> Left ObjectNoSection
    Just _
      | power < 0 || power > 30 -> Left (ObjectInvalidAlignment power)
      | BS.null fill -> Left (ObjectInvalidInput "empty alignment fill")
      | otherwise -> Right ()
  let boundary = 1 `shiftL` power
      size = sectionSize (draftCurrent draft)
      padding = (boundary - size `mod` boundary) `mod` boundary
      fillRange phase count =
        let prefix = BS.take count (BS.drop phase fill)
            (whole, suffix) = (count - BS.length prefix) `divMod` BS.length fill
         in BS.concat (prefix : replicate whole fill <> [BS.take suffix fill])
      fullChunk = fillRange 0 65536
      append phase remaining = when (remaining > 0) $ do
        let count = fromIntegral (min 65536 remaining)
            bytes = if phase == 0 && count == 65536 then fullChunk else fillRange phase count
        modifyObject writer (addItem (Bytes bytes))
        append ((phase + count) `mod` BS.length fill) (remaining - fromIntegral count)
  modifyObject writer $ \current ->
    pure current {draftCurrent = (draftCurrent current) {sectionAlignment = max power (sectionAlignment (draftCurrent current))}}
  append 0 padding

checked :: Either ObjectError value -> IO value
checked = either (ioError . userError . show) pure

removeIfPresent :: FilePath -> IO ()
removeIfPresent path = do
  exists <- doesFileExist path
  when exists (removeFile path)
