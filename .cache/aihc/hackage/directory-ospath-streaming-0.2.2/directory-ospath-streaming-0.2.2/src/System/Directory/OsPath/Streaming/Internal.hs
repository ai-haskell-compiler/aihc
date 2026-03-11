-- |
-- Module:     System.Directory.OsPath.Streaming.Internal
-- Copyright:  (c) Sergey Vinokurov 2024
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE BangPatterns   #-}
{-# LANGUAGE MagicHash      #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecursiveDo    #-}
{-# LANGUAGE UnboxedTuples  #-}

module System.Directory.OsPath.Streaming.Internal
  ( DirStream(..)
  , openDirStream
  , readDirStream
  , closeDirStream

  , readDirStreamWithCache
  ) where

import Control.Concurrent.Counter (Counter)
import qualified Control.Concurrent.Counter as Counter
import Control.Monad (when)
import System.Mem.Weak (Weak, mkWeak, finalize)
import System.OsPath (OsPath)

import qualified System.Directory.OsPath.Streaming.Internal.Raw as Raw
import System.Directory.OsPath.Types
import System.Directory.OsPath.Utils (touch)

-- | Abstract handle to directory contents.
--
-- May be closed multiple times and will be automatically closed by GC
-- when it goes out of scope.
data DirStream = DirStream
  { dsHandle   :: !Raw.RawDirStream
  , dsIsClosed :: {-# UNPACK #-} !Counter
  , dsFin      :: {-# UNPACK #-} !(Weak DirStream)
  }

openDirStream :: OsPath -> IO DirStream
openDirStream root = mdo
  dsHandle   <- Raw.openRawDirStream root
  dsIsClosed <- Counter.new 0
  let stream = DirStream{dsHandle, dsIsClosed, dsFin}
  dsFin <- mkWeak stream stream (Just (closeDirStreamInternal stream))
  pure stream

-- | Deallocate directory handle. It’s safe to close 'DirStream' multiple times,
-- unlike the underlying OS-specific directory stream handle.
closeDirStream :: DirStream -> IO ()
closeDirStream stream = do
  -- Finalize ourselves to do it only once instead of running finalizer
  -- in GC afterwards once more.
  finalize (dsFin stream)
  touch stream

closeDirStreamInternal :: DirStream -> IO ()
closeDirStreamInternal DirStream{dsHandle, dsIsClosed} = do
  !oldVal <- Counter.cas dsIsClosed 0 1
  when (oldVal == 0) $
    Raw.closeRawDirStream dsHandle

readDirStream :: DirStream -> IO (Maybe (OsPath, FileType))
readDirStream = Raw.readRawDirStream . dsHandle

readDirStreamWithCache
  :: Raw.DirReadCache
  -> DirStream
  -> IO (Maybe (OsPath, Basename OsPath, FileType))
readDirStreamWithCache cache =
  Raw.readRawDirStreamWithCache cache . dsHandle
