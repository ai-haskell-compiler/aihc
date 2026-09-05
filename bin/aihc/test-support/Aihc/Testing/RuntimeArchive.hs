{-# LANGUAGE LambdaCase #-}

-- | One runtime archive per target, collector and set of C arguments, shared
-- by every link in the test executable.
--
-- A link needs the runtime as an archive rather than as a list of sources, so
-- a test stays independent of which runtime units are C and which are Lir. See
-- the "Runtime units" section of @docs/lir.md@. Building that archive once per
-- link would compile the whole runtime once per link, so the archives live in
-- one directory for the lifetime of the test executable and 'releaseCachedRuntimeArchives'
-- removes them at the end.
module Aihc.Testing.RuntimeArchive
  ( cachedRuntimeArchive,
    releaseCachedRuntimeArchives,
  )
where

import Aihc.Cli.Runtime (RuntimeBuild, buildRuntimeArchive)
import Aihc.Native (NativeTarget, RuntimeGarbageCollector)
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newEmptyMVar, newMVar, putMVar, readMVar)
import Control.Exception (SomeException, throwIO, try)
import Data.Foldable (traverse_)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import System.Directory (removeDirectoryRecursive)
import System.IO.Error (tryIOError)
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import System.IO.Unsafe (unsafePerformIO)

-- | What makes two runtime archives interchangeable.
type ArchiveKey = (NativeTarget, RuntimeGarbageCollector, [String])

-- | The archive of each key, or the failure that building it raised. A key
-- that is being built holds an empty cell, so a second caller waits for the
-- first rather than building the same archive again.
archiveCache :: MVar (Map ArchiveKey (MVar (Either SomeException RuntimeBuild)))
archiveCache = unsafePerformIO (newMVar Map.empty)
{-# NOINLINE archiveCache #-}

-- | The directory that holds every cached archive, created on first use.
archiveRoot :: MVar (Maybe FilePath)
archiveRoot = unsafePerformIO (newMVar Nothing)
{-# NOINLINE archiveRoot #-}

-- | The runtime archive for one target, collector and set of extra C
-- arguments. The first caller of a key builds it and every later caller of
-- that key gets the same archive. A build that fails is remembered, so a
-- broken toolchain raises once per key instead of once per link.
cachedRuntimeArchive :: NativeTarget -> RuntimeGarbageCollector -> [String] -> IO RuntimeBuild
cachedRuntimeArchive target garbageCollector extraCArguments = do
  let key = (target, garbageCollector, extraCArguments)
  slot <-
    modifyMVar archiveCache $ \cache ->
      case Map.lookup key cache of
        Just existing -> pure (cache, Right existing)
        Nothing -> do
          fresh <- newEmptyMVar
          pure (Map.insert key fresh cache, Left fresh)
  case slot of
    Right ready -> readMVar ready >>= either throwIO pure
    Left fresh -> do
      result <- try (buildForKey key)
      putMVar fresh result
      either throwIO pure result

buildForKey :: ArchiveKey -> IO RuntimeBuild
buildForKey (target, garbageCollector, extraCArguments) = do
  root <- runtimeArchiveRoot
  directory <- createTempDirectory root "archive"
  buildRuntimeArchive target garbageCollector extraCArguments directory

runtimeArchiveRoot :: IO FilePath
runtimeArchiveRoot =
  modifyMVar archiveRoot $ \case
    Just root -> pure (Just root, root)
    Nothing -> do
      temporary <- getCanonicalTemporaryDirectory
      root <- createTempDirectory temporary "aihc-runtime-archives"
      pure (Just root, root)

-- | Forget and remove the cached archives. The test executable calls this
-- when it exits.
releaseCachedRuntimeArchives :: IO ()
releaseCachedRuntimeArchives = do
  modifyMVar_ archiveCache (const (pure Map.empty))
  root <- modifyMVar archiveRoot (\current -> pure (Nothing, current))
  traverse_ (tryIOError . removeDirectoryRecursive) root
