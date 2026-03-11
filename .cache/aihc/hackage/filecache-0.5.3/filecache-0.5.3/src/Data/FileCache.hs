{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- This module let you create caches where keys are file names, and values are automatically expired when the file is modified for any reason.
--
-- This is usually done in the following fashion :
--
-- > cache <- newFileCache
-- > o <- query cache "/path/to/file" computation
--
-- The computation will be used to populate the cache if this call results in a miss. The result is forced to WHNM.
module Data.FileCache (FileCache, FileCacheR, newFileCache, killFileCache, invalidate, query, OnModified, queryWith, getCache, lazyQuery) where

import Data.FileCache.Internal
