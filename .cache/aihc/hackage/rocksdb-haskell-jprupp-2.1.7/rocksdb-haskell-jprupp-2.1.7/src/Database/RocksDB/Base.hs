{-# LANGUAGE LambdaCase #-}
-- |
-- Module      : Database.RocksDB.Base
-- Copyright   : (c) 2012-2013 The leveldb-haskell Authors
--               (c) 2014-2020 The rocksdb-haskell Authors
-- License     : BSD3
-- Maintainer  : jprupp@protonmail.ch
-- Stability   : experimental
-- Portability : non-portable
--
-- RocksDB Haskell binding.
--
-- The API closely follows the C-API of RocksDB.

module Database.RocksDB.Base
    ( -- * Exported Types
      DB (..)
    , BatchOp (..)
    , Range
    , ColumnFamily

    -- * Options
    , Config (..)

    -- * Basic Database Manipulations
    , withDB
    , withDBCF
    , put
    , putCF
    , delete
    , deleteCF
    , write
    , get
    , getCF
    , withSnapshot
    , snapshot
    , createSnapshot
    , releaseSnapshot

    -- * Administrative Functions
    , Property (..), getProperty
    , destroy
    , repair
    , approximateSize

    -- * Iteration
    , module Database.RocksDB.Iterator
    ) where

import           Control.Monad             (forM, when, (>=>))
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as BS
import           Data.ByteString.Internal  (ByteString (..))
import qualified Data.ByteString.Unsafe    as BU
import           Database.RocksDB.C
import           Database.RocksDB.Internal
import           Database.RocksDB.Iterator
import           UnliftIO
import           UnliftIO.Directory
import           UnliftIO.Foreign
import           UnliftIO.Resource

-- | Properties exposed by RocksDB
data Property = NumFilesAtLevel Int | Stats | SSTables
    deriving (Eq, Show)

data BatchOp = Put !ByteString !ByteString
             | Del !ByteString
             | PutCF !ColumnFamily !ByteString !ByteString
             | DelCF !ColumnFamily !ByteString
             deriving (Eq, Show)

-- | Open a database.
--
-- The returned handle will be automatically released with 'close'
-- when the function exits.
withDB :: MonadUnliftIO m => FilePath -> Config -> (DB -> m a) -> m a
withDB path config f =
    withOptions config $ \opts_ptr ->
    withReadOpts Nothing $ \read_opts ->
    withWriteOpts $ \write_opts ->
    bracket (create_db opts_ptr read_opts write_opts) destroy_db f
  where
    destroy_db db = liftIO $
        c_rocksdb_close $ rocksDB db
    create_db opts_ptr read_opts write_opts = do
        when (createIfMissing config) $
            createDirectoryIfMissing True path
        withCString path $ \path_ptr -> do
            db_ptr <- liftIO . throwIfErr "open" $
                c_rocksdb_open opts_ptr path_ptr
            return DB { rocksDB = db_ptr
                      , columnFamilies = []
                      , readOpts = read_opts
                      , writeOpts = write_opts
                      }

withDBCF :: MonadUnliftIO m
         => FilePath
         -> Config
         -> [(String, Config)]
         -> (DB -> m a)
         -> m a
withDBCF path config cf_cfgs f =
    withOptions config $ \opts_ptr ->
    withOptionsCF (map snd cf_cfgs) $ \cf_opts ->
    withReadOpts Nothing $ \read_opts ->
    withWriteOpts $ \write_opts ->
    withStrings (map fst cf_cfgs) $ \cf_names ->
    allocaArray (length cf_cfgs + 1) $ \cf_names_array ->
    allocaArray (length cf_cfgs + 1) $ \cf_opts_array ->
    allocaArray (length cf_cfgs + 1) $ \cf_ptrs_array ->
        bracket ( create_db opts_ptr
                            cf_names
                            cf_names_array
                            cf_opts
                            cf_opts_array
                            read_opts
                            cf_ptrs_array
                            write_opts
                ) destroy_db f
  where
    create_new cf_names cf_opts opts_ptr read_opts write_opts = do
        createDirectoryIfMissing True path
        withCString path $ \path_ptr -> do
            db_ptr <- liftIO . throwIfErr "open" $
                c_rocksdb_open opts_ptr path_ptr
            cfs <- forM (zip cf_names cf_opts) $ \(n, o) ->
                throwIfErr "create_column_family" $
                c_rocksdb_create_column_family
                db_ptr o n
            return DB { rocksDB = db_ptr
                      , columnFamilies = cfs
                      , readOpts = read_opts
                      , writeOpts = write_opts
                      }
    destroy_db db = liftIO $ do
        mapM_ c_rocksdb_column_family_handle_destroy (columnFamilies db)
        c_rocksdb_close $ rocksDB db
    create_db opts_ptr
              cf_names
              cf_names_array
              cf_opts
              cf_opts_array
              read_opts
              cf_ptrs_array
              write_opts = liftIO $ do
        when (createIfMissing config) $
            createDirectoryIfMissing True path
        listDirectory path >>= \case
            [] -> create_new cf_names cf_opts opts_ptr read_opts write_opts
            _ -> withCString path $ \path_ptr ->
                withCString "default" $ \cf_deflt_name -> do
                    pokeArray cf_names_array (cf_deflt_name : cf_names)
                    pokeArray cf_opts_array (opts_ptr : cf_opts)
                    db_ptr <- throwIfErr "open" $
                        c_rocksdb_open_column_families
                        opts_ptr
                        path_ptr
                        (intToCInt (length cf_cfgs + 1))
                        cf_names_array
                        cf_opts_array
                        cf_ptrs_array
                    cfs <- peekArray (length cf_cfgs + 1) cf_ptrs_array
                    return DB { rocksDB = db_ptr
                              , columnFamilies = tail cfs
                              , readOpts = read_opts
                              , writeOpts = write_opts
                              }

-- | Run an action with a snapshot of the database.
-- The 'DB' object is not valid after the action ends.
withSnapshot :: MonadUnliftIO m => DB -> (DB -> m a) -> m a
withSnapshot db f =
    bracket (createSnapshot db) releaseSnapshot (f . fst)

-- | The 'DB' snapshot is not valid outside of 'MonadResource'.
snapshot :: (MonadIO m, MonadResource m) => DB -> m DB
snapshot db =
    fst . snd <$> allocate (createSnapshot db) releaseSnapshot

-- | Manually create an unmanaged snapshot.
createSnapshot :: MonadIO m => DB -> m (DB, Snapshot)
createSnapshot db@DB{rocksDB = db_ptr} = liftIO $ do
    snap_ptr <- c_rocksdb_create_snapshot db_ptr
    withReadOpts (Just snap_ptr) $ \read_opts ->
        return (db{readOpts = read_opts}, snap_ptr)

-- | Function to release an unmanaged snapshot.
releaseSnapshot :: MonadIO m => (DB, Snapshot) -> m ()
releaseSnapshot (DB{rocksDB = db_ptr}, snap_ptr) =
    liftIO $ c_rocksdb_release_snapshot db_ptr snap_ptr

-- | Get a DB property.
getProperty :: MonadIO m => DB -> Property -> m (Maybe ByteString)
getProperty DB{rocksDB = db_ptr} p = liftIO $
    withCString (prop p) $
    c_rocksdb_property_value db_ptr >=> \case
    val_ptr | val_ptr == nullPtr -> return Nothing
            | otherwise -> do
                  res <- Just <$> BS.packCString val_ptr
                  freeCString val_ptr
                  return res
  where
    prop (NumFilesAtLevel i) = "rocksdb.num-files-at-level" ++ show i
    prop Stats               = "rocksdb.stats"
    prop SSTables            = "rocksdb.sstables"

-- | Destroy the given RocksDB database.
destroy :: MonadIO m => FilePath -> Options -> m ()
destroy path opts_ptr = liftIO $
    withCString path $ \path_ptr ->
    throwIfErr "destroy" $ c_rocksdb_destroy_db opts_ptr path_ptr

-- | Repair the given RocksDB database.
repair :: MonadIO m => FilePath -> Options -> m ()
repair path opts_ptr = liftIO $
    withCString path $ \path_ptr ->
    throwIfErr "repair" $ c_rocksdb_repair_db opts_ptr path_ptr


-- TODO: support [Range], like C API does
type Range  = (ByteString, ByteString)

-- | Inspect the approximate sizes of the different levels.
approximateSize :: MonadIO m => DB -> Range -> m Int64
approximateSize DB{rocksDB = db_ptr} (from, to) = liftIO $
    BU.unsafeUseAsCStringLen from $ \(from_ptr, flen) ->
    BU.unsafeUseAsCStringLen to   $ \(to_ptr, tlen)   ->
    withArray [from_ptr]          $ \from_ptrs        ->
    withArray [intToCSize flen]   $ \flen_ptrs        ->
    withArray [to_ptr]            $ \to_ptrs          ->
    withArray [intToCSize tlen]   $ \tlen_ptrs        ->
    allocaArray 1                 $ \size_ptrs        -> do
        c_rocksdb_approximate_sizes db_ptr 1
                                    from_ptrs flen_ptrs
                                    to_ptrs tlen_ptrs
                                    size_ptrs
        fmap head $ peekArray 1 size_ptrs >>= mapM toInt64

    where
        toInt64 = return . fromIntegral

-- | Write a key/value pair.
put :: MonadIO m => DB -> ByteString -> ByteString -> m ()
put db = putCommon db Nothing

putCF :: MonadIO m => DB -> ColumnFamily -> ByteString -> ByteString -> m ()
putCF db cf = putCommon db (Just cf)

putCommon :: MonadIO m => DB -> Maybe ColumnFamily -> ByteString -> ByteString -> m ()
putCommon DB{rocksDB = db_ptr, writeOpts = write_opts} mcf key value = liftIO $
    BU.unsafeUseAsCStringLen key   $ \(key_ptr, klen) ->
    BU.unsafeUseAsCStringLen value $ \(val_ptr, vlen) ->
        throwIfErr "put" $ case mcf of
            Just cf -> c_rocksdb_put_cf
                      db_ptr write_opts cf
                      key_ptr (intToCSize klen)
                      val_ptr (intToCSize vlen)
            Nothing -> c_rocksdb_put
                      db_ptr write_opts
                      key_ptr (intToCSize klen)
                      val_ptr (intToCSize vlen)

-- | Read a value by key.
get :: MonadIO m => DB -> ByteString -> m (Maybe ByteString)
get db = getCommon db Nothing

getCF :: MonadIO m => DB -> ColumnFamily -> ByteString -> m (Maybe ByteString)
getCF db cf = getCommon db (Just cf)

getCommon :: MonadIO m => DB -> Maybe ColumnFamily -> ByteString -> m (Maybe ByteString)
getCommon DB{rocksDB = db_ptr, readOpts = read_opts} mcf key = liftIO $
    BU.unsafeUseAsCStringLen key $ \(key_ptr, klen) ->
    alloca                       $ \vlen_ptr -> do
        val_ptr <- throwIfErr "get" $
            case mcf of
                Just cf -> c_rocksdb_get_cf
                           db_ptr read_opts cf
                           key_ptr (intToCSize klen) vlen_ptr
                Nothing -> c_rocksdb_get
                           db_ptr read_opts
                           key_ptr (intToCSize klen) vlen_ptr
        if val_ptr == nullPtr
            then return Nothing
            else do
                vlen <- peek vlen_ptr
                res <- BU.unsafePackMallocCStringLen (val_ptr, cSizeToInt vlen)
                return $ Just res

delete :: MonadIO m => DB -> ByteString -> m ()
delete db = deleteCommon db Nothing

deleteCF :: MonadIO m => DB -> ColumnFamily -> ByteString -> m ()
deleteCF db cf = deleteCommon db (Just cf)

-- | Delete a key/value pair.
deleteCommon :: MonadIO m => DB -> Maybe ColumnFamily -> ByteString -> m ()
deleteCommon DB{rocksDB = db_ptr, writeOpts = write_opts} mcf key = liftIO $
    BU.unsafeUseAsCStringLen key $ \(key_ptr, klen) ->
    throwIfErr "delete" $ case mcf of
    Just cf -> c_rocksdb_delete_cf db_ptr write_opts cf key_ptr (intToCSize klen)
    Nothing -> c_rocksdb_delete db_ptr write_opts key_ptr (intToCSize klen)

-- | Perform a batch mutation.
write :: MonadIO m => DB -> [BatchOp] -> m ()
write DB{rocksDB = db_ptr, writeOpts = write_opts} batch = liftIO $
    bracket
    c_rocksdb_writebatch_create
    c_rocksdb_writebatch_destroy $ \batch_ptr -> do
        mapM_ (batchAdd batch_ptr) batch
        throwIfErr "write" $ c_rocksdb_write db_ptr write_opts batch_ptr
        -- ensure @ByteString@s (and respective shared @CStringLen@s) aren't
        -- GC'ed until here
        mapM_ touch batch
  where
    batchAdd batch_ptr (Put key val) =
        BU.unsafeUseAsCStringLen key $ \(key_ptr, klen) ->
        BU.unsafeUseAsCStringLen val $ \(val_ptr, vlen) ->
            c_rocksdb_writebatch_put
            batch_ptr
            key_ptr (intToCSize klen)
            val_ptr (intToCSize vlen)
    batchAdd batch_ptr (PutCF cf_ptr key val) =
        BU.unsafeUseAsCStringLen key $ \(key_ptr, klen) ->
        BU.unsafeUseAsCStringLen val $ \(val_ptr, vlen) ->
            c_rocksdb_writebatch_put_cf
            batch_ptr
            cf_ptr
            key_ptr (intToCSize klen)
            val_ptr (intToCSize vlen)
    batchAdd batch_ptr (Del key) =
        BU.unsafeUseAsCStringLen key $ \(key_ptr, klen) ->
            c_rocksdb_writebatch_delete
            batch_ptr
            key_ptr (intToCSize klen)
    batchAdd batch_ptr (DelCF cf_ptr key) =
        BU.unsafeUseAsCStringLen key $ \(key_ptr, klen) ->
            c_rocksdb_writebatch_delete_cf
            batch_ptr
            cf_ptr
            key_ptr (intToCSize klen)
    touch (Put (PS p _ _) (PS p' _ _)) = do
        touchForeignPtr p
        touchForeignPtr p'
    touch (PutCF _ (PS p _ _) (PS p' _ _)) = do
        touchForeignPtr p
        touchForeignPtr p'
    touch (Del (PS p _ _)) =
        touchForeignPtr p
    touch (DelCF _ (PS p _ _)) =
        touchForeignPtr p

withStrings :: MonadUnliftIO m => [String] -> ([CString] -> m a) -> m a
withStrings ss f =
    go [] ss
  where
    go acc []     = f (reverse acc)
    go acc (x:xs) = withCString x $ \p -> go (p:acc) xs
