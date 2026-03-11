-- |
-- Module:     System.Directory.OsPath.Streaming.Internal.Raw
-- Copyright:  (c) Sergey Vinokurov 2024
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com
--
-- Streaming functions for interacting with the filesystem.
--
-- These do the basic job of reading directory entries but care must
-- be taken to not close these streams more than once.

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE UnboxedTuples       #-}

#ifndef mingw32_HOST_OS
# if MIN_VERSION_unix(2, 8, 6) && __GLASGOW_HASKELL__ >= 902
#  define HAVE_UNIX_CACHE 1
# endif
#endif

module System.Directory.OsPath.Streaming.Internal.Raw
  ( RawDirStream(..)
  , openRawDirStream
  , readRawDirStream
  , closeRawDirStream

  , DirReadCache(..)
  , allocateDirReadCache
  , releaseDirReadCache
  , readRawDirStreamWithCache
  ) where

import System.OsPath (osp, (</>))

import System.Directory.OsPath.FileType
import System.Directory.OsPath.Types

#ifdef mingw32_HOST_OS
import Control.Concurrent.Counter (Counter)
import qualified Control.Concurrent.Counter as Counter
import Control.Monad (unless)
import System.OsPath.Types (OsPath)
import System.OsString.Internal.Types (OsString(OsString), getOsString)
import System.OsString.Windows (pstr)
import qualified System.Win32.Types as Win32
import qualified System.Win32.WindowsString.File as Win32
#endif

-- Don’t use #else to make treesitter do better job - it parses #else part as comments.
#ifndef mingw32_HOST_OS
import System.OsPath.Types (OsPath)
import System.OsString.Internal.Types (OsString(OsString), getOsString)
import qualified System.Posix.Directory.PosixPath as Posix

# ifdef HAVE_UNIX_CACHE
import Data.Coerce (coerce)
import Foreign.C (CString, CChar)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (sizeOf, alignment, peekElemOff)
import qualified System.Posix.Directory.Internals as DirInternals
import System.Posix.PosixPath.FilePath (peekFilePath)

import GHC.Exts (MutableByteArray#, newAlignedPinnedByteArray#, mutableByteArrayContents#, RealWorld)
import GHC.IO (IO(..))
import GHC.Int (Int(..))
import GHC.Ptr (Ptr(..))

import System.Directory.OsPath.Utils (touch)
# endif
#endif

-- | Abstract handle to directory contents.
--
-- Not thread safe and shouldn't be closed more than once.

#ifdef mingw32_HOST_OS
data RawDirStream = RawDirStream !Win32.HANDLE !Win32.FindData !Counter !OsPath
#endif
#ifndef mingw32_HOST_OS
data RawDirStream = RawDirStream !Posix.DirStream !OsPath
#endif

openRawDirStream :: OsPath -> IO RawDirStream
#ifdef mingw32_HOST_OS
openRawDirStream fp = do
  (h, fdat) <- Win32.findFirstFile $ getOsString fp <> [pstr|\*|]
  hasMore <- Counter.new 1 -- always at least two records, "." and ".."
  pure $! RawDirStream h fdat hasMore fp
#endif

#ifndef mingw32_HOST_OS
openRawDirStream root = do
  stream <- Posix.openDirStream (getOsString root)
  pure $ RawDirStream stream root
#endif

-- | Deallocate directory handle. It’s not safe to call multiple times
-- on the same handle.
closeRawDirStream :: RawDirStream -> IO ()

#ifdef mingw32_HOST_OS
closeRawDirStream (RawDirStream h _ _ _) = Win32.findClose h
#endif
#ifndef mingw32_HOST_OS
closeRawDirStream (RawDirStream stream _) = Posix.closeDirStream stream
#endif

readRawDirStream :: RawDirStream -> IO (Maybe (OsPath, FileType))
readRawDirStream stream = do
  cache <- allocateDirReadCache
  res   <- readRawDirStreamWithCache cache stream
  -- Safe to don’t care about exceptions because we know that cache is
  -- just a byte vector so just touch# it for now.
  releaseDirReadCache cache
  pure $ (\(_, Basename x, typ) -> (x, typ)) <$> res

#ifdef mingw32_HOST_OS
-- No state on Windows
newtype DirReadCache = DirReadCache ()
#endif

#ifndef mingw32_HOST_OS

# ifndef HAVE_UNIX_CACHE
-- No state in early unix package
newtype DirReadCache = DirReadCache ()
# endif

# ifdef HAVE_UNIX_CACHE
data DirReadCache = DirReadCache (MutableByteArray# RealWorld)
# endif

#endif


allocateDirReadCache :: IO DirReadCache
#ifdef mingw32_HOST_OS
allocateDirReadCache = pure $ DirReadCache ()
#endif

#ifndef mingw32_HOST_OS
# ifndef HAVE_UNIX_CACHE
allocateDirReadCache = pure $ DirReadCache ()
# endif
# ifdef HAVE_UNIX_CACHE
allocateDirReadCache = IO $ \s0 ->
  case newAlignedPinnedByteArray# size align s0 of
    (# s1, mbarr# #) ->
      (# s1, DirReadCache mbarr# #)
  where
    !(I# size)  = sizeOf    (undefined :: Ptr DirInternals.DirEnt)
    !(I# align) = alignment (undefined :: Ptr DirInternals.DirEnt)
# endif
#endif


releaseDirReadCache :: DirReadCache -> IO ()
#ifdef mingw32_HOST_OS
releaseDirReadCache _ = pure ()
#endif
#ifndef mingw32_HOST_OS

# ifndef HAVE_UNIX_CACHE
releaseDirReadCache _ = pure ()
# endif
# ifdef HAVE_UNIX_CACHE
releaseDirReadCache = touch
# endif
#endif


readRawDirStreamWithCache
  :: DirReadCache
  -> RawDirStream
  -> IO (Maybe (OsPath, Basename OsPath, FileType))
#ifdef mingw32_HOST_OS
readRawDirStreamWithCache _ stream@(RawDirStream _ _ _ root) = do
  traverse (\x -> let full = root </> x in (full, Basename x,) <$> getFileType full) =<< _readRawDirStreamSimple stream
#endif
#ifndef mingw32_HOST_OS

# ifndef HAVE_UNIX_CACHE
readRawDirStreamWithCache _ stream@(RawDirStream _ root) = do
  traverse (\x -> let full = root </> x in (full, Basename x,) <$> getFileType full) =<< _readRawDirStreamSimple stream
# endif
# ifdef HAVE_UNIX_CACHE
readRawDirStreamWithCache (DirReadCache barr#) (RawDirStream stream root) = go
  where
    cache :: Ptr DirInternals.DirEnt
    cache = Ptr (mutableByteArrayContents# barr#)

    shouldSkipDirEntry :: CString -> IO Bool
    shouldSkipDirEntry ptr
      | ptr == nullPtr = pure True
    shouldSkipDirEntry ptr = do
      (x1 :: CChar) <- peekElemOff ptr 0
      case x1 of
        0  -> pure False
        46 -> do -- ASCII for ‘.’
          (x2 :: CChar) <- peekElemOff ptr 1
          case x2 of
            0  -> pure True
            46 -> do -- ASCII for ‘.’
              (x3 :: CChar) <- peekElemOff ptr 2
              pure $! x3 == 0
            _  -> pure False
        _  -> pure False

    go :: IO (Maybe (OsPath, Basename OsPath, FileType))
    go = do
      x <- DirInternals.readDirStreamWithPtr
        cache
        (\dirEnt -> do
          (namePtr :: CString) <- DirInternals.dirEntName dirEnt

          shouldSkip <- shouldSkipDirEntry namePtr

          if shouldSkip
          then
            pure Nothing
          else do
            !path <- peekFilePath namePtr

            let fullPath = root </> coerce path

            !typ  <- DirInternals.dirEntType dirEnt

            typ' <- case typ of
              DirInternals.UnknownType         -> getFileType fullPath
              DirInternals.NamedPipeType       -> pure regularOther
              DirInternals.CharacterDeviceType -> pure regularOther
              DirInternals.DirectoryType       -> pure regularDirectory
              DirInternals.BlockDeviceType     -> pure regularOther
              DirInternals.RegularFileType     -> pure regularFile
              DirInternals.SymbolicLinkType    -> getFileType fullPath
              DirInternals.SocketType          -> pure regularOther
              DirInternals.WhiteoutType        -> pure regularOther
              -- Unaccounted type, probably should not happeen since the
              -- list above is exhaustive.
              _                                -> getFileType fullPath

            pure (Just (fullPath, Basename $ coerce path, typ')))
        stream

      case x of
        Nothing           -> pure Nothing
        Just Nothing      -> go
        Just res@(Just _) -> pure res
# endif
#endif

_readRawDirStreamSimple :: RawDirStream -> IO (Maybe OsPath)

#ifdef mingw32_HOST_OS
_readRawDirStreamSimple (RawDirStream h fdat hasMore _) = go
  where
    go = do
      hasMore' <- Counter.get hasMore
      if hasMore' /= 0
      then do
        filename  <- Win32.getFindDataFileName fdat
        hasMore'' <- Win32.findNextFile h fdat
        unless hasMore'' $
          Counter.set hasMore 0
        if filename == getOsString [osp|.|] || filename == getOsString [osp|..|]
        then go
        else pure $ Just $ OsString filename
      else pure Nothing
#endif
#ifndef mingw32_HOST_OS
_readRawDirStreamSimple (RawDirStream stream _) = go
  where
# ifndef HAVE_UNIX_CACHE
    go = do
      fp <- Posix.readDirStream stream
      case () of
        _ | fp == mempty
          -> pure Nothing
          | fp == getOsString [osp|.|] || fp == getOsString [osp|..|]
          -> go
          | otherwise
          -> pure $ Just $ OsString fp
# endif
# ifdef HAVE_UNIX_CACHE
    go = do
      fp <- Posix.readDirStreamMaybe stream
      case fp of
        Nothing -> pure Nothing
        Just fp'
          | fp' == getOsString [osp|.|] || fp' == getOsString [osp|..|]
          -> go
          | otherwise
          -> pure $ Just $ OsString fp'
# endif
#endif

