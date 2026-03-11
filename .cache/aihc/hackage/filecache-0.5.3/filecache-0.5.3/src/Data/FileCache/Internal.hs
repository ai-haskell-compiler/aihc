{-# LANGUAGE ScopedTypeVariables #-}

-- |
--
-- Internal module ... use at your own risks!
module Data.FileCache.Internal where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Catch
import qualified Data.Strict.Either as R
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.String
import Data.Time.Clock (getCurrentTime)
import Debug.Trace
import System.Directory (canonicalizePath)
import System.FSNotify
import System.FilePath (addTrailingPathSeparator, takeDirectory)
import Prelude

-- | The main FileCache type, for queries returning 'Either r a'. The r
-- type must be an instance of 'Error'.
data FileCacheR r a
  = FileCache
  { _cache :: TVar (M.Map FilePath (R.Either r a, OnModified)),
    _watchedDirs :: TVar (M.Map FilePath (S.Set FilePath, StopListening)),
    _manager :: WatchManager,
    _channel :: EventChannel,
    _tid :: TVar (Maybe ThreadId)
  }

-- | A default type synonym, for String errors.
type FileCache = FileCacheR String

-- | Hook to invoke after an entry is removed from the cache
-- (because the corresponding file has been modified)
type OnModified = IO ()

-- | Generates a new file cache. The opaque type is for use with other
-- functions.
newFileCache :: IO (FileCacheR r a)
newFileCache = do
  c <- newChan
  tcache <- newTVarIO M.empty
  wcache <- newTVarIO M.empty
  manager <- startManager
  tid <- forkIO $ forever $ do
    e <- readChan c
    let cfp = eventPath e
        dir = addTrailingPathSeparator (takeDirectory cfp)
    join $ atomically $ do
      onModified <- maybe (return ()) snd . M.lookup cfp <$> readTVar tcache
      modifyTVar tcache $ M.delete cfp
      wdirs <- readTVar wcache
      (onModified >>) <$> case M.lookup dir wdirs of
        Nothing -> return $ return ()
        Just (watched, stop) ->
          let watched' = S.delete cfp watched
           in if S.null watched'
                then stop <$ modifyTVar wcache (M.delete dir)
                else return () <$ modifyTVar wcache (M.insert dir (watched', stop))
  FileCache tcache wcache manager c <$> newTVarIO (Just tid)

-- | Destroys the thread running the FileCache. Pretty dangerous stuff.
killFileCache :: FileCacheR r a -> IO ()
killFileCache (FileCache tcache twatched mgr _ tid) = do
  atomically $ do
    writeTVar tcache M.empty
    writeTVar twatched M.empty
    writeTVar tid Nothing
  stopManager mgr

-- | Manually invalidates an entry.
invalidate :: FilePath -> FileCacheR e a -> IO ()
invalidate fp c = do
  cfp <- canon fp
  tm <- getCurrentTime
  writeChan (_channel c) (Removed cfp tm IsFile)

canon :: FilePath -> IO FilePath
canon fp = canonicalizePath fp `catchAll` const (return fp)

-- | Queries the cache, populating it if necessary, returning a strict
-- 'Either' (from "Data.Either.Strict").
--
-- Queries that fail with an 'IOExeception' will not create a cache entry.
query ::
  forall e a.
  (IsString e) =>
  FileCacheR e a ->
  -- | Path of the file entry
  FilePath ->
  -- | The computation that will be used to populate the cache
  IO (R.Either e a) ->
  IO (R.Either e a)
query q fp = queryWith q fp (return ())

-- | Generalization of 'query' that allows to specify an 'OnModified' hook
queryWith ::
  forall e a.
  (IsString e) =>
  FileCacheR e a ->
  -- | Path of the file entry
  FilePath ->
  OnModified ->
  -- | The computation that will be used to populate the cache
  IO (R.Either e a) ->
  IO (R.Either e a)
queryWith f@(FileCache tcache twatched wm chan tmtid) fp onModified action = do
  mtid <- readTVarIO tmtid
  case mtid of
    Nothing -> return (R.Left (fromString "Closed cache"))
    Just _ -> do
      canonical <- canon fp
      mp <- getCache f
      case M.lookup canonical mp of
        Just x -> return x
        Nothing ->
          (action >>= withWatch canonical)
            `catchIOError` (return . R.Left . fromString . show)
            `catchAll` (withWatch canonical . R.Left . fromString . show)
      where
        withWatch :: FilePath -> R.Either e a -> IO (R.Either e a)
        withWatch canonical value = value <$ (addWatch canonical value `catchAll` traceShowM)
        addWatch canonical value = join $ atomically $ do
          let cpath = addTrailingPathSeparator (takeDirectory canonical)
          modifyTVar tcache (M.insert canonical (value, onModified))
          watched <- readTVar twatched
          case M.lookup cpath watched of
            Nothing -> return $ do
              stop <- watchDirChan wm cpath (const True) chan
              atomically (modifyTVar twatched (M.insert cpath (S.singleton canonical, stop)))
            Just (wfiles, stop) ->
              return () <$ modifyTVar twatched (M.insert cpath (S.insert canonical wfiles, stop))

-- | Just like `query`, but with the standard "Either" type. Note that it
-- is just there for easy interoperability with the more comme "Either"
-- type, as the result is still forced.
lazyQuery ::
  (IsString r) =>
  FileCacheR r a ->
  -- | Path of the file entry
  FilePath ->
  -- | The computation that will be used to populate the cache
  IO (Either r a) ->
  IO (Either r a)
lazyQuery q fp generate = fmap unstrict (query q fp (fmap strict generate))
  where
    strict (Left x) = R.Left x
    strict (Right x) = R.Right x
    unstrict (R.Left x) = Left x
    unstrict (R.Right x) = Right x

-- | Gets a copy of the cache.
getCache :: FileCacheR e a -> IO (M.Map FilePath (R.Either e a))
getCache = atomically . fmap (fmap fst) . readTVar . _cache
