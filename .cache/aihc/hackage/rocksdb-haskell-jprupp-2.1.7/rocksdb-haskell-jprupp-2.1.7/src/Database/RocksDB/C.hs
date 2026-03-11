{-# LANGUAGE CPP                      #-}
{-# LANGUAGE EmptyDataDecls           #-}
{-# LANGUAGE ForeignFunctionInterface #-}
-- |
-- Module      : Database.RocksDB.C
-- Copyright   : (c) 2012-2020 The rocksdb-haskell Authors
-- License     : BSD3
-- Maintainer  : jprupp@protonmail.ch
-- Stability   : experimental
-- Portability : non-portable
--

module Database.RocksDB.C
    ( RocksDB
    , Options
    , ReadOpts
    , WriteOpts
    , ColumnFamily
    , SliceTransform
    , Snapshot
    , Iterator
    , WriteBatch
    , FilterPolicy
    , ErrPtr
    , DBName
    , CFName
    , Key
    , Val
    , c_rocksdb_open
    , c_rocksdb_open_column_families
    , c_rocksdb_close
    , c_rocksdb_create_column_family
    , c_rocksdb_drop_column_family
    , c_rocksdb_column_family_handle_destroy
    , c_rocksdb_put
    , c_rocksdb_put_cf
    , c_rocksdb_delete
    , c_rocksdb_delete_cf
    , c_rocksdb_write
    , c_rocksdb_get
    , c_rocksdb_get_cf
    , c_rocksdb_multi_get
    , c_rocksdb_multi_get_cf
    , c_rocksdb_create_snapshot
    , c_rocksdb_release_snapshot
    , c_rocksdb_property_value
    , c_rocksdb_approximate_sizes
    , c_rocksdb_destroy_db
    , c_rocksdb_repair_db
    , c_rocksdb_create_iterator
    , c_rocksdb_create_iterator_cf
    , c_rocksdb_iter_destroy
    , c_rocksdb_iter_valid
    , c_rocksdb_iter_seek_to_first
    , c_rocksdb_iter_seek_to_last
    , c_rocksdb_iter_seek
    , c_rocksdb_iter_next
    , c_rocksdb_iter_prev
    , c_rocksdb_iter_key
    , c_rocksdb_iter_value
    , c_rocksdb_iter_get_error
    , c_rocksdb_writebatch_create
    , c_rocksdb_writebatch_destroy
    , c_rocksdb_writebatch_clear
    , c_rocksdb_writebatch_put
    , c_rocksdb_writebatch_put_cf
    , c_rocksdb_writebatch_delete
    , c_rocksdb_writebatch_delete_cf
    , c_rocksdb_options_create
    , c_rocksdb_options_destroy
    , c_rocksdb_options_set_create_if_missing
    , c_rocksdb_options_set_error_if_exists
    , c_rocksdb_options_set_paranoid_checks
    , c_rocksdb_options_set_max_open_files
    , c_rocksdb_options_set_prefix_extractor
    , c_rocksdb_options_set_block_based_table_factory
    , c_rocksdb_block_based_options_create
    , c_rocksdb_block_based_options_set_filter_policy
    , c_rocksdb_block_based_options_destroy
    , c_rocksdb_slicetransform_create_fixed_prefix
    , c_rocksdb_slicetransform_destroy
    , c_rocksdb_filterpolicy_destroy
    , c_rocksdb_filterpolicy_create_bloom_full
    , c_rocksdb_readoptions_create
    , c_rocksdb_readoptions_destroy
    , c_rocksdb_readoptions_set_snapshot
    , c_rocksdb_writeoptions_create
    , c_rocksdb_writeoptions_destroy
    , c_rocksdb_free
    ) where

import           Foreign
import           Foreign.C.String
import           Foreign.C.Types

data LRocksDB
data LColumnFamily
data LIterator
data LOptions
data LBlockBasedOptions
data LReadOpts
data LSnapshot
data LWriteBatch
data LWriteOpts
data LFilterPolicy
data LSliceTransform

type RocksDB            = Ptr LRocksDB
type ColumnFamily       = Ptr LColumnFamily
type Options            = Ptr LOptions
type WriteBatch         = Ptr LWriteBatch
type SliceTransform     = Ptr LSliceTransform
type ReadOpts           = Ptr LReadOpts
type WriteOpts          = Ptr LWriteOpts
type Snapshot           = Ptr LSnapshot
type Iterator           = Ptr LIterator
type FilterPolicy       = Ptr LFilterPolicy
type BlockBasedOptions  = Ptr LBlockBasedOptions

type ErrPtr           = Ptr CString
type DBName           = CString
type CFName           = CString
type Key              = CString
type Val              = CString

foreign import ccall safe "rocksdb/c.h rocksdb_open"
  c_rocksdb_open :: Options -> DBName -> ErrPtr -> IO RocksDB

foreign import ccall safe "rocksdb/c.h rocksdb_open_column_families"
  c_rocksdb_open_column_families :: Options
                                 -> DBName
                                 -> CInt
                                 -> Ptr CFName
                                 -> Ptr Options
                                 -> Ptr ColumnFamily
                                 -> ErrPtr
                                 -> IO RocksDB

foreign import ccall safe "rocksdb/c.h rocksdb_close"
  c_rocksdb_close :: RocksDB -> IO ()

foreign import ccall safe "rocksdb/c.h rocksdb_create_column_family"
  c_rocksdb_create_column_family :: RocksDB
                                 -> Options
                                 -> CFName
                                 -> ErrPtr
                                 -> IO ColumnFamily

foreign import ccall safe "rocksdb/c.h rocksdb_drop_column_family"
  c_rocksdb_drop_column_family :: RocksDB
                               -> ColumnFamily
                               -> ErrPtr
                               -> IO ()

foreign import ccall safe "rocksdb/c.h rocksdb_column_family_handle_destroy"
  c_rocksdb_column_family_handle_destroy :: ColumnFamily -> IO ()

foreign import ccall safe "rocksdb/c.h rocksdb_put"
  c_rocksdb_put :: RocksDB
                -> WriteOpts
                -> Key -> CSize
                -> Val -> CSize
                -> ErrPtr
                -> IO ()

foreign import ccall safe "rocksdb/c.h rocksdb_put_cf"
  c_rocksdb_put_cf :: RocksDB
                   -> WriteOpts
                   -> ColumnFamily
                   -> Key -> CSize
                   -> Val -> CSize
                   -> ErrPtr
                   -> IO ()

foreign import ccall safe "rocksdb/c.h rocksdb_delete"
  c_rocksdb_delete :: RocksDB
                   -> WriteOpts
                   -> Key -> CSize
                   -> ErrPtr
                   -> IO ()

foreign import ccall safe "rocksdb/c.h rocksdb_delete_cf"
  c_rocksdb_delete_cf :: RocksDB
                      -> WriteOpts
                      -> ColumnFamily
                      -> Key -> CSize
                      -> ErrPtr
                      -> IO ()

foreign import ccall safe "rocksdb/c.h rocksdb_write"
  c_rocksdb_write :: RocksDB
                  -> WriteOpts
                  -> WriteBatch
                  -> ErrPtr
                  -> IO ()

-- | Returns NULL if not found. A malloc()ed array otherwise.
-- Stores the length of the array in *vallen.
foreign import ccall safe "rocksdb/c.h rocksdb_get"
  c_rocksdb_get :: RocksDB
                -> ReadOpts
                -> Key -> CSize
                -> Ptr CSize        -- ^ value length
                -> ErrPtr
                -> IO CString

foreign import ccall safe "rocksdb/c.h rocksdb_get_cf"
  c_rocksdb_get_cf :: RocksDB
                   -> ReadOpts
                   -> ColumnFamily
                   -> Key -> CSize
                   -> Ptr CSize        -- ^ value length
                   -> ErrPtr
                   -> IO CString

foreign import ccall safe "rocksdb/c.h rocksdb_multi_get"
  c_rocksdb_multi_get :: RocksDB
                      -> ReadOpts
                      -> CSize        -- ^ number of keys
                      -> Ptr CString  -- ^ keys
                      -> Ptr CSize    -- ^ key sizes
                      -> Ptr CString  -- ^ values
                      -> Ptr CSize    -- ^ values sizes
                      -> ErrPtr
                      -> IO ()

foreign import ccall safe "rocksdb/c.h rocksdb_multi_get_cf"
  c_rocksdb_multi_get_cf :: RocksDB
                         -> ReadOpts
                         -> Ptr ColumnFamily -- ^ column families
                         -> CSize                   -- ^ number of keys
                         -> Ptr CString             -- ^ keys
                         -> Ptr CSize               -- ^ key sizes
                         -> Ptr CString             -- ^ values
                         -> Ptr CSize               -- ^ values sizes
                         -> ErrPtr
                         -> IO ()

foreign import ccall safe "rocksdb/c.h rocksdb_create_snapshot"
  c_rocksdb_create_snapshot :: RocksDB -> IO Snapshot

foreign import ccall safe "rocksdb/c.h rocksdb_release_snapshot"
  c_rocksdb_release_snapshot :: RocksDB -> Snapshot -> IO ()

-- | Returns NULL if property name is unknown. Else returns a pointer to a
-- malloc()-ed null-terminated value.
foreign import ccall safe "rocksdb/c.h rocksdb_property_value"
  c_rocksdb_property_value :: RocksDB -> CString -> IO CString

foreign import ccall safe "rocksdb/c.h rocksdb_approximate_sizes"
  c_rocksdb_approximate_sizes :: RocksDB
                              -> CInt
                                 -- ^ num ranges
                              -> Ptr CString -> Ptr CSize
                                 -- ^ range start keys (array)
                              -> Ptr CString -> Ptr CSize
                                 -- ^ range limit keys (array)
                              -> Ptr Word64
                                 -- ^ array of approx. sizes of ranges
                              -> IO ()

foreign import ccall safe "rocksdb/c.h rocksdb_destroy_db"
  c_rocksdb_destroy_db :: Options -> DBName -> ErrPtr -> IO ()

foreign import ccall safe "rocksdb/c.h rocksdb_repair_db"
  c_rocksdb_repair_db :: Options -> DBName -> ErrPtr -> IO ()


--
-- Iterator
--

foreign import ccall safe "rocksdb/c.h rocksdb_create_iterator"
  c_rocksdb_create_iterator :: RocksDB
                            -> ReadOpts
                            -> IO Iterator

foreign import ccall safe "rocksdb/c.h rocksdb_create_iterator_cf"
  c_rocksdb_create_iterator_cf :: RocksDB
                               -> ReadOpts
                               -> ColumnFamily
                               -> IO Iterator

foreign import ccall safe "rocksdb/c.h rocksdb_iter_destroy"
  c_rocksdb_iter_destroy :: Iterator -> IO ()

foreign import ccall safe "rocksdb/c.h rocksdb_iter_valid"
  c_rocksdb_iter_valid :: Iterator -> IO CUChar

foreign import ccall safe "rocksdb/c.h rocksdb_iter_seek_to_first"
  c_rocksdb_iter_seek_to_first :: Iterator -> IO ()

foreign import ccall safe "rocksdb/c.h rocksdb_iter_seek_to_last"
  c_rocksdb_iter_seek_to_last :: Iterator -> IO ()

foreign import ccall safe "rocksdb/c.h rocksdb_iter_seek"
  c_rocksdb_iter_seek :: Iterator -> Key -> CSize -> IO ()

foreign import ccall safe "rocksdb/c.h rocksdb_iter_next"
  c_rocksdb_iter_next :: Iterator -> IO ()

foreign import ccall safe "rocksdb/c.h rocksdb_iter_prev"
  c_rocksdb_iter_prev :: Iterator -> IO ()

foreign import ccall safe "rocksdb/c.h rocksdb_iter_key"
  c_rocksdb_iter_key :: Iterator -> Ptr CSize -> IO Key

foreign import ccall safe "rocksdb/c.h rocksdb_iter_value"
  c_rocksdb_iter_value :: Iterator -> Ptr CSize -> IO Val

foreign import ccall safe "rocksdb/c.h rocksdb_iter_get_error"
  c_rocksdb_iter_get_error :: Iterator -> ErrPtr -> IO ()


--
-- Write batch
--

foreign import ccall safe "rocksdb/c.h rocksdb_writebatch_create"
  c_rocksdb_writebatch_create :: IO WriteBatch

foreign import ccall safe "rocksdb/c.h rocksdb_writebatch_destroy"
  c_rocksdb_writebatch_destroy :: WriteBatch -> IO ()

foreign import ccall safe "rocksdb/c.h rocksdb_writebatch_clear"
  c_rocksdb_writebatch_clear :: WriteBatch -> IO ()

foreign import ccall safe "rocksdb/c.h rocksdb_writebatch_put"
  c_rocksdb_writebatch_put :: WriteBatch
                           -> Key -> CSize
                           -> Val -> CSize
                           -> IO ()

foreign import ccall safe "rocksdb/c.h rocksdb_writebatch_put_cf"
  c_rocksdb_writebatch_put_cf :: WriteBatch
                              -> ColumnFamily
                              -> Key -> CSize
                              -> Val -> CSize
                              -> IO ()

foreign import ccall safe "rocksdb/c.h rocksdb_writebatch_delete"
  c_rocksdb_writebatch_delete :: WriteBatch
                              -> Key -> CSize -> IO ()

foreign import ccall safe "rocksdb/c.h rocksdb_writebatch_delete_cf"
  c_rocksdb_writebatch_delete_cf :: WriteBatch
                                 -> ColumnFamily
                                 -> Key
                                 -> CSize
                                 -> IO ()

--
-- Options
--

foreign import ccall safe "rocksdb/c.h rocksdb_options_create"
  c_rocksdb_options_create :: IO Options

foreign import ccall "rocksdb/c.h rocksdb_options_destroy"
  c_rocksdb_options_destroy :: Options -> IO ()

foreign import ccall safe "rocksdb/c.h rocksdb_options_set_create_if_missing"
  c_rocksdb_options_set_create_if_missing :: Options -> CBool -> IO ()

foreign import ccall safe "rocksdb/c.h rocksdb_options_set_error_if_exists"
  c_rocksdb_options_set_error_if_exists :: Options -> CBool -> IO ()

foreign import ccall safe "rocksdb/c.h rocksdb_options_set_paranoid_checks"
  c_rocksdb_options_set_paranoid_checks :: Options -> CBool -> IO ()

foreign import ccall safe "rocksdb/c.h rocksdb_options_set_max_open_files"
  c_rocksdb_options_set_max_open_files :: Options -> CInt -> IO ()

foreign import ccall safe "rocksdb/c.h rocksdb_options_set_prefix_extractor"
  c_rocksdb_options_set_prefix_extractor :: Options
                                         -> SliceTransform
                                         -> IO ()

--
-- Block-Based Options
--

foreign import ccall safe "rocksdb/c.h rocksdb_block_based_options_create"
  c_rocksdb_block_based_options_create :: IO BlockBasedOptions

foreign import ccall safe "rocksdb/c.h rocksdb_block_based_options_destroy"
  c_rocksdb_block_based_options_destroy :: BlockBasedOptions
                                        -> IO ()

foreign import ccall safe "rocksdb/c.h rocksdb_block_based_options_set_filter_policy"
  c_rocksdb_block_based_options_set_filter_policy :: BlockBasedOptions
                                                  -> FilterPolicy
                                                  -> IO ()

foreign import ccall safe "rocksdb/c.h rocksdb_options_set_block_based_table_factory"
  c_rocksdb_options_set_block_based_table_factory :: Options
                                                  -> BlockBasedOptions
                                                  -> IO ()

--
-- Prefix Extractor
--

foreign import ccall safe "rocksdb/c.h rocksdb_slicetransform_create_fixed_prefix"
  c_rocksdb_slicetransform_create_fixed_prefix :: CSize -> IO SliceTransform

foreign import ccall safe "rocksdb/c.h rocksdb_slicetransform_destroy"
  c_rocksdb_slicetransform_destroy :: SliceTransform -> IO ()

--
-- Bloom Filter
--

foreign import ccall safe "rocksdb/c.h rocksdb_filterpolicy_destroy"
  c_rocksdb_filterpolicy_destroy :: FilterPolicy -> IO ()

foreign import ccall safe "rocksdb/c.h rocksdb_filterpolicy_create_bloom_full"
  c_rocksdb_filterpolicy_create_bloom_full :: CInt -> IO FilterPolicy

--
-- Read options
--

foreign import ccall safe "rocksdb/c.h rocksdb_readoptions_create"
  c_rocksdb_readoptions_create :: IO ReadOpts

foreign import ccall safe "rocksdb/c.h rocksdb_readoptions_destroy"
  c_rocksdb_readoptions_destroy :: ReadOpts -> IO ()

foreign import ccall safe "rocksdb/c.h rocksdb_readoptions_set_snapshot"
  c_rocksdb_readoptions_set_snapshot :: ReadOpts -> Snapshot -> IO ()


--
-- Write options
--

foreign import ccall safe "rocksdb/c.h rocksdb_writeoptions_create"
  c_rocksdb_writeoptions_create :: IO WriteOpts

foreign import ccall safe "rocksdb/c.h rocksdb_writeoptions_destroy"
  c_rocksdb_writeoptions_destroy :: WriteOpts -> IO ()

--
-- Free
--

foreign import ccall safe "rocksdb/c.h rocksdb_free"
  c_rocksdb_free :: CString -> IO ()
