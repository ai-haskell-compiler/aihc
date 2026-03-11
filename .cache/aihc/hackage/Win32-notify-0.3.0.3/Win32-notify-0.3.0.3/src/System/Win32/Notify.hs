module System.Win32.Notify
  ( Event(..)
  , EventVariety(..)
  , Handler
  , WatchId(..)
  , WatchManager(..)
  , initWatchManager
  , killWatch
  , killWatchManager
  , watch
  , watchDirectory
    ) where

import Control.Concurrent
import Control.Concurrent.MVar
import Data.Bits
import Data.List (intersect)
import Data.Map (Map)
import System.Directory
import System.Win32 (closeHandle)
import System.Win32.File
import System.Win32.FileNotify
import qualified Data.Map as Map

data EventVariety
    = Modify
    | Move
    | Create
    | Delete
  deriving Eq

data Event
    -- | A file was modified. @Modified isDirectory file@
    = Modified
        { isDirectory :: Bool
        , filePath :: FilePath
        }
    -- TODO: Problems with receiving (oldName, nil), (nil, newName) events at
    -- unpredictable times mean that, for now, rename detection is disabled.
    {-
    -- A file was moved within the directory.
    | Renamed
        { isDirectory :: Bool
        , oldName     :: Maybe FilePath
        , newName     :: Maybe FilePath
        }
    -}
    -- | A file was created. @Created isDirectory file@
    | Created
        { isDirectory :: Bool
        , filePath :: FilePath
        }
    -- | A file was deleted. @Deleted isDirectory file@
    | Deleted
        { isDirectory :: Bool
        , filePath :: FilePath
        }
    deriving (Eq, Show)

type Handler = Event -> IO ()

data WatchId = WatchId ThreadId ThreadId Handle deriving (Eq, Ord, Show)
type WatchMap = Map WatchId Handler
data WatchManager = WatchManager (MVar WatchMap)

void :: IO ()
void = return ()

initWatchManager :: IO WatchManager
initWatchManager =  do
  mvarMap <- newMVar Map.empty
  return (WatchManager mvarMap)

killWatchManager :: WatchManager -> IO ()
killWatchManager (WatchManager mvarMap) = do
  watchMap <- readMVar mvarMap
  flip mapM_ (Map.keys watchMap) $ killWatch

varietiesToFnFlags :: [EventVariety] -> FileNotificationFlag
varietiesToFnFlags = foldl (.|.) 0 . map evToFnFlag'
    where evToFnFlag' :: EventVariety -> FileNotificationFlag
          evToFnFlag' ev = case ev of
            Modify  -> fILE_NOTIFY_CHANGE_LAST_WRITE
            Move    -> fILE_NOTIFY_CHANGE_FILE_NAME .|. fILE_NOTIFY_CHANGE_DIR_NAME
            Create  -> fILE_NOTIFY_CHANGE_FILE_NAME .|. fILE_NOTIFY_CHANGE_DIR_NAME
            Delete  -> fILE_NOTIFY_CHANGE_FILE_NAME .|. fILE_NOTIFY_CHANGE_DIR_NAME

-- watchDirectoryOnce :: FilePath -> Bool -> [EventVariety] -> IO
-- watchDirectoryOnce dir wst evs = do
--         h <- getWatchHandle dir
--         readDirectoryChanges h wst (evToFnFlag evs) >>= actsToEvent

watchDirectory :: WatchManager -> FilePath -> Bool -> [EventVariety] -> Handler -> IO WatchId
watchDirectory (WatchManager mvarMap) dir watchSubTree varieties handler = do
  watchHandle <- getWatchHandle dir
  chanEvents <- newChan
  tid1 <- forkIO $ dispatcher chanEvents
  tid2 <- forkIO $ osEventsReader watchHandle chanEvents
  modifyMVar_ mvarMap $ \watchMap -> return (Map.insert (WatchId tid1 tid2 watchHandle) handler watchMap)
  return (WatchId tid1 tid2 watchHandle)
  where
    dispatcher :: Chan [Event] -> IO ()
    dispatcher chanEvents = do
      events <- readChan chanEvents
      mapM_ maybeHandle events
      dispatcher chanEvents
    osEventsReader :: Handle -> Chan [Event] -> IO ()
    osEventsReader watchHandle chanEvents = do
      events <- (readDirectoryChanges watchHandle watchSubTree (varietiesToFnFlags varieties) >>= actsToEvents)
      writeChan chanEvents events
      osEventsReader watchHandle chanEvents
    maybeHandle :: Handler
    maybeHandle event =
      if (==) (eventToVariety event) `any` varieties then handler event else void

watch :: WatchManager -> FilePath -> Bool -> [EventVariety] -> IO (WatchId, Chan [Event])
watch (WatchManager mvarMap) dir watchSubTree varieties = do
  watchHandle <- getWatchHandle dir
  chanEvents <- newChan
  tid <- forkIO $ osEventsReader watchHandle chanEvents
  modifyMVar_ mvarMap $ \watchMap -> return (Map.insert (WatchId tid tid watchHandle) (\_ -> void) watchMap)
  return ((WatchId tid tid watchHandle), chanEvents)
  where
    osEventsReader :: Handle -> Chan [Event] -> IO ()
    osEventsReader watchHandle chanEvents = do
      events <- (readDirectoryChanges watchHandle watchSubTree (varietiesToFnFlags varieties) >>= actsToEvents)
      writeChan chanEvents events
      osEventsReader watchHandle chanEvents

killWatch :: WatchId -> IO ()
killWatch (WatchId tid1 tid2 handle) = do
    killThread tid1
    if tid1 /= tid2 then killThread tid2 else void
    closeHandle handle

eventToVariety :: Event -> EventVariety
eventToVariety event = case event of
  Created  _ _   -> Create
  Deleted  _ _   -> Delete
  Modified _ _   -> Modify
  -- Renamed  _ _ _ -> [Move]

actsToEvents :: [(Action, String)] -> IO [Event]
actsToEvents = mapM actToEvent
  where
    actToEvent (act, fn) = do
      isDir <- doesDirectoryExist fn
      case act of
        FileModified    -> return $ Modified isDir fn
        FileAdded       -> return $ Created  isDir fn
        FileRemoved     -> return $ Deleted  isDir fn
        FileRenamedOld  -> return $ Deleted  isDir fn
        FileRenamedNew  -> return $ Created  isDir fn
-- actsToEvent [(FileRenamedOld, fnold),(FileRenamedNew, fnnew)] = do
--     isDir <- doesDirectoryExist fnnew
--     return $ Renamed isDir (Just fnold) fnnew
