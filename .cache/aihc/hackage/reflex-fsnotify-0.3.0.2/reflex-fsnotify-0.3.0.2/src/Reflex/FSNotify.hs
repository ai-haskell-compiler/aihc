{-|
Module: Reflex.FSNotify
Description: Watch for filesystem changes in reflex
-}

{-# LANGUAGE FlexibleContexts #-}
module Reflex.FSNotify
  ( watchDirectory
  , watchDir
  , watchDirs
  , watchTree
  , wrapWatch
  , listDirectories
  , watchDirectoryTree
  , FSEvent
  ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Reflex
import qualified System.FSNotify as FS

import Data.Set (Set)
import qualified Data.Set as Set
import System.Directory
import System.FilePath ((</>))

-- A type synonym to disambiguate Reflex 'Event's from 'System.FSNotify.Event'
type FSEvent = FS.Event

-- | Watch a directory for changes
{-# DEPRECATED watchDirectory "Use `watchDir cfg path (const True)` instead" #-}
watchDirectory
  :: (Reflex t, TriggerEvent t m, PerformEvent t m, MonadIO (Performable m))
  => FS.WatchConfig
  -> Event t FilePath
  -> m (Event t FS.Event)
watchDirectory cfg path = watchDir cfg path (const True)

wrapWatch
  :: (Reflex t, TriggerEvent t m, PerformEvent t m, MonadIO (Performable m))
  => (FS.WatchManager -> pathinfo -> FS.Action -> IO a)
  -> FS.WatchConfig
  -> Event t pathinfo
  -> m (Event t FSEvent)
wrapWatch f cfg path =
  performEventAsync $ ffor path $ \p cb -> liftIO $ void $ forkIO $
    FS.withManagerConf cfg $ \mgr -> do
      _ <- f mgr p cb
      forever $ threadDelay 1000000

watchDir
  :: (Reflex t, TriggerEvent t m, PerformEvent t m, MonadIO (Performable m))
  => FS.WatchConfig
  -> Event t FilePath
  -> FS.ActionPredicate
  -> m (Event t FSEvent)
watchDir cfg path evFilter = wrapWatch (\mgr p -> FS.watchDir mgr p evFilter) cfg path

watchDirs
  :: (Reflex t, TriggerEvent t m, PerformEvent t m, MonadIO (Performable m))
  => FS.WatchConfig
  -> Event t [FilePath]
  -> FS.ActionPredicate
  -> m (Event t FSEvent)
watchDirs cfg path evFilter = wrapWatch (\mgr ps cb -> forM_ ps $ \p ->
  FS.watchDir mgr p evFilter cb) cfg path

watchTree
  :: (Reflex t, TriggerEvent t m, PerformEvent t m, MonadIO (Performable m))
  => FS.WatchConfig
  -> Event t FilePath
  -> FS.ActionPredicate
  -> m (Event t FSEvent)
watchTree cfg path evFilter = wrapWatch (\mgr p -> FS.watchTree mgr p evFilter) cfg path

listDirectories
  :: FilePath
  -> IO (Set FilePath)
listDirectories start = do
  start' <- canonicalizePath start
  Set.insert start' <$> listDirectories' Set.empty start'
  where
    listDirectories' :: Set FilePath -> FilePath -> IO (Set FilePath)
    listDirectories' seen dir0 = do
      let canonicalize p = canonicalizePath $ dir0 </> p
      contents <- mapM canonicalize =<< listDirectory dir0
      dirs <- filterM doesDirectoryExist contents
      let newDirs = filter (not . flip Set.member seen) dirs
          newSeen = Set.union seen $ Set.fromList newDirs
      allDirs <- mapM (listDirectories' newSeen) newDirs
      return $ Set.unions $ Set.fromList dirs : allDirs

-- | Like 'watchTree' except that it tries to avoid symlink loops and calls
-- 'watchDir' on each directory found
watchDirectoryTree
  :: (Reflex t, TriggerEvent t m, PerformEvent t m, MonadIO (Performable m))
  => FS.WatchConfig
  -> Event t FilePath
  -> FS.ActionPredicate
  -> m (Event t FSEvent)
watchDirectoryTree cfg root evFilter =
  let f mgr p cb = do
        dirs <- listDirectories p
        mapM_ (\dir -> FS.watchDir mgr dir evFilter cb) dirs
  in wrapWatch f cfg root
