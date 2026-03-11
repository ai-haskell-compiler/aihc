{-# LANGUAGE ImportQualifiedPost #-}

-- |
-- Module      : Database.RocksDB.Iterator
-- Copyright   : (c) 2012-2013 The leveldb-haskell Authors
--               (c) 2014-2020 The rocksdb-haskell Authors
-- License     : BSD3
-- Maintainer  : jprupp@protonmail.ch
-- Stability   : experimental
-- Portability : non-portable
--
-- Iterating over key ranges.
module Database.RocksDB.Iterator
  ( Iterator,
    withIter,
    withIterCF,
    iter,
    iterCF,
    iterator,
    createIterator,
    destroyIterator,
    iterEntry,
    iterFirst,
    iterGetError,
    iterKey,
    iterLast,
    iterNext,
    iterPrev,
    iterSeek,
    iterValid,
    iterValue,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Unsafe qualified as BU
import Database.RocksDB.C
import Database.RocksDB.Internal
import Foreign
import Foreign.C.Error (throwErrnoIfNull)
import Foreign.C.String (CString)
import Foreign.C.Types (CSize)
import UnliftIO
import UnliftIO.Resource

-- | Create 'Iterator' and use it.
--
-- Note that an 'Iterator' creates a snapshot of the database implicitly, so
-- updates written after the iterator was created are not visible. You may,
-- however, specify an older 'Snapshot' in the 'ReadOptions'.
--
-- Iterator should not be used after computation ends.
withIter :: (MonadUnliftIO m) => DB -> (Iterator -> m a) -> m a
withIter db = withIterCommon db Nothing

withIterCF :: (MonadUnliftIO m) => DB -> ColumnFamily -> (Iterator -> m a) -> m a
withIterCF db cf = withIterCommon db (Just cf)

-- | Variation on 'iterator' below.
iter :: (MonadIO m, MonadResource m) => DB -> m Iterator
iter db = iterator db Nothing

iterCF :: (MonadIO m, MonadResource m) => DB -> ColumnFamily -> m Iterator
iterCF db cf = iterator db (Just cf)

withIterCommon ::
  (MonadUnliftIO m) =>
  DB ->
  Maybe ColumnFamily ->
  (Iterator -> m a) ->
  m a
withIterCommon DB {rocksDB = rocks_db, readOpts = read_opts} mcf =
  bracket create_iterator destroy_iterator
  where
    destroy_iterator = liftIO . c_rocksdb_iter_destroy
    create_iterator = liftIO $
      throwErrnoIfNull "create_iterator" $ case mcf of
        Just cf -> c_rocksdb_create_iterator_cf rocks_db read_opts cf
        Nothing -> c_rocksdb_create_iterator rocks_db read_opts

-- | Iterator is not valid outside of 'ResourceT' context.
iterator ::
  (MonadIO m, MonadResource m) =>
  DB ->
  Maybe ColumnFamily ->
  m Iterator
iterator db mcf =
  snd <$> allocate (createIterator db mcf) destroyIterator

-- | Manually create unmanaged iterator.
createIterator :: (MonadIO m) => DB -> Maybe ColumnFamily -> m Iterator
createIterator DB {rocksDB = rocks_db, readOpts = read_opts} mcf = liftIO $
  throwErrnoIfNull "create_iterator" $ case mcf of
    Just cf -> c_rocksdb_create_iterator_cf rocks_db read_opts cf
    Nothing -> c_rocksdb_create_iterator rocks_db read_opts

-- | Destroy unmanaged iterator.
destroyIterator :: (MonadIO m) => Iterator -> m ()
destroyIterator = liftIO . c_rocksdb_iter_destroy

-- | An iterator is either positioned at a key/value pair, or not valid. This
-- function returns /true/ iff the iterator is valid.
iterValid :: (MonadIO m) => Iterator -> m Bool
iterValid iter_ptr = liftIO $ do
  x <- c_rocksdb_iter_valid iter_ptr
  return (x /= 0)

-- | Position at the first key in the source that is at or past target. The
-- iterator is /valid/ after this call iff the source contains an entry that
-- comes at or past target.
iterSeek :: (MonadIO m) => Iterator -> ByteString -> m ()
iterSeek iter_ptr key = liftIO $
  BU.unsafeUseAsCStringLen key $ \(key_ptr, klen) ->
    c_rocksdb_iter_seek iter_ptr key_ptr (intToCSize klen)

-- | Position at the first key in the source. The iterator is /valid/ after this
-- call iff the source is not empty.
iterFirst :: (MonadIO m) => Iterator -> m ()
iterFirst = liftIO . c_rocksdb_iter_seek_to_first

-- | Position at the last key in the source. The iterator is /valid/ after this
-- call iff the source is not empty.
iterLast :: (MonadIO m) => Iterator -> m ()
iterLast = liftIO . c_rocksdb_iter_seek_to_last

-- | Moves to the next entry in the source. After this call, 'iterValid' is
-- /true/ iff the iterator was not positioned at the last entry in the source.
--
-- If the iterator is not valid, this function does nothing. Note that this is a
-- shortcoming of the C API: an 'iterPrev' might still be possible, but we can't
-- determine if we're at the last or first entry.
iterNext :: (MonadIO m) => Iterator -> m ()
iterNext iter_ptr = liftIO $ c_rocksdb_iter_next iter_ptr

-- | Moves to the previous entry in the source. After this call, 'iterValid' is
-- /true/ iff the iterator was not positioned at the first entry in the source.
--
-- If the iterator is not valid, this function does nothing. Note that this is a
-- shortcoming of the C API: an 'iterNext' might still be possible, but we can't
-- determine if we're at the last or first entry.
iterPrev :: (MonadIO m) => Iterator -> m ()
iterPrev iter_ptr = liftIO $ c_rocksdb_iter_prev iter_ptr

-- | Return the key for the current entry if the iterator is currently
-- positioned at an entry, ie. 'iterValid'.
iterKey :: (MonadIO m) => Iterator -> m (Maybe ByteString)
iterKey it = liftIO $ iterString it c_rocksdb_iter_key

-- | Return the value for the current entry if the iterator is currently
-- positioned at an entry, ie. 'iterValid'.
iterValue :: (MonadIO m) => Iterator -> m (Maybe ByteString)
iterValue it = liftIO $ iterString it c_rocksdb_iter_value

-- | Return the current entry as a pair, if the iterator is currently positioned
-- at an entry, ie. 'iterValid'.
iterEntry :: (MonadIO m) => Iterator -> m (Maybe (ByteString, ByteString))
iterEntry it = liftIO $ do
  mkey <- iterKey it
  mval <- iterValue it
  return $ (,) <$> mkey <*> mval

-- | Check for errors
--
-- Note that this captures somewhat severe errors such as a corrupted database.
iterGetError :: (MonadIO m) => Iterator -> m (Maybe ByteString)
iterGetError iter_ptr = liftIO $ alloca $ \err_ptr -> do
  poke err_ptr nullPtr
  c_rocksdb_iter_get_error iter_ptr err_ptr
  err_str <- peek err_ptr
  if err_str == nullPtr
    then return Nothing
    else Just <$> BU.unsafePackMallocCString err_str

--
-- Internal
--

iterString ::
  Iterator ->
  (Iterator -> Ptr CSize -> IO CString) ->
  IO (Maybe ByteString)
iterString it f = do
  valid <- iterValid it
  if valid
    then alloca $ \len_ptr -> do
      str_ptr <- f it len_ptr
      if str_ptr == nullPtr
        then return Nothing
        else do
          len <- peek len_ptr
          Just <$> B.packCStringLen (str_ptr, cSizeToInt len)
    else return Nothing