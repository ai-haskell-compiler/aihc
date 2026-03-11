{-|
A library for accessing a Koji hub via its XMLRPC API.
-}

module Distribution.Koji
       ( BuildID(..)
       , BuildInfo(..)
       , buildIDInfo
       , fedoraKojiHub
       , centosKojiHub
       , kojiBuildTags
       , kojiBuildTarget
       , kojiGetBuildID
       , kojiGetBuildState
       , kojiGetBuildTaskID
       , kojiGetCurrentRepo
       , kojiGetRepo
       , kojiGetTaskInfo
       , kojiGetTaskChildren
       , kojiGetTaskState
       , kojiGetUserID
       , kojiLatestBuild
       , kojiLatestBuildRepo
       , kojiListSideTags
       , kojiListTaskIDs
       , kojiUserBuildTasks
       , KojiBuild(..)
       , kojiListTaggedBuilds
       , PackageID(..)
       , TagID(..)
       , TaskID(..)
       , UserID(..)
       , displayID
       , getID
       , readID
       , readID'
       , TaskState(..)
       , getTaskState
       , openTaskStates
       , openTaskValues
       , readTaskState
       , BuildState(..)
       , readBuildState
       , Struct
       , lookupStruct
       , Value (..)
       , getInt
       , getString
       , RepoState(..)
       , readRepoState
       )
where

import qualified Data.List as L
import Data.Maybe
import Network.XmlRpc.Internals

import Distribution.Koji.API

-- | A class for various id's: taskid, tagid, buildid, packageid, etc
class ID a where
  getID :: a -> Int
  mkID :: Int -> a

displayID :: ID a => a -> String
displayID = show . getID

-- FIXME rename to structID ?
readID :: ID a => Struct -> Maybe a
readID st = lookup "id" st >>= fmap mkID . getInt

readID' :: Struct -> Maybe Int
readID' st = lookup "id" st >>= getInt

-- FIXME rename to valueInt ?
getInt :: Value -> Maybe Int
getInt (ValueInt i) = Just i
getInt _ = Nothing

getString :: Value -> Maybe String
getString (ValueString i) = Just i
getString _ = Nothing

newtype TaskID = TaskId Int
  deriving Show

instance ID TaskID where
  getID (TaskId i) = i
  mkID = TaskId

newtype TagID = TagId Int
  deriving Show

instance ID TagID where
  getID (TagId i) = i
  mkID = TagId

newtype UserID = UserId Int
  deriving Show

instance ID UserID where
  getID (UserId i) = i
  mkID = UserId

newtype BuildID = BuildId Int

instance ID BuildID where
  getID (BuildId i) = i
  mkID = BuildId

newtype PackageID = PackageId Int

instance ID PackageID where
  getID (PackageId i) = i
  mkID = PackageId

newtype BuildrootID = BuildrootId Int

instance ID BuildrootID where
  getID (BuildrootId i) = i
  mkID = BuildrootId

data BuildInfo = BuildInfoID Int | BuildInfoNVR String

-- | map a BuildInfo into a Info
buildInfo :: BuildInfo -> Info
buildInfo (BuildInfoID bid) = InfoID bid
buildInfo (BuildInfoNVR nvr) = InfoString nvr

-- | map a buildid into a buildinfo
buildIDInfo :: BuildID -> BuildInfo
buildIDInfo (BuildId bid) = BuildInfoID bid

-- | main Fedora Koji Hub
fedoraKojiHub :: String
fedoraKojiHub = "https://koji.fedoraproject.org/kojihub"

-- | Centos Koji mbox Hub
centosKojiHub :: String
centosKojiHub = "https://koji.mbox.centos.org/kojihub"

-- | Get the buildid of an nvr build
kojiGetBuildID :: String -- ^ hub url
               -> String -- ^ NVR
               -> IO (Maybe BuildID)
kojiGetBuildID hubUrl nvr =
  ((fmap BuildId . lookupStruct "id") =<<) <$> getBuild hubUrl (InfoString nvr)

-- | Get the task of an nvr build
kojiGetBuildTaskID :: String -- ^ hub url
                   -> String -- ^ NVR
                   -> IO (Maybe TaskID)
kojiGetBuildTaskID hubUrl nvr =
  ((fmap TaskId . lookupStruct "task_id") =<<) <$> getBuild hubUrl (InfoString nvr)

-- | List tasks filtered by query options
kojiListTaskIDs :: String -- ^ hub url
                -> Struct -- ^ options
                -> Struct -- ^ query opts
                -> IO [TaskID]
kojiListTaskIDs hubUrl opts qopts =
  mapMaybe readID <$> listTasks hubUrl opts qopts

-- | List the open tasks of a user (matching source/target)
kojiUserBuildTasks :: String -- ^ hub url
                   -> UserID
                   -> Maybe String -- ^ source
                   -> Maybe String -- ^ target
                   -> IO [TaskID]
kojiUserBuildTasks hubUrl userid msource mtarget = do
  tasks <- listTasks hubUrl [("owner",ValueInt (getID userid)),("method",ValueString "build"),("state",openTaskValues)] []
  return $ map TaskId . mapMaybe (lookupStruct "id") $ filter isTheBuild tasks
  where
    isTheBuild :: Struct -> Bool
    isTheBuild st =
      let mreq = lookupStruct "request" st in
        case mreq of
          Nothing -> False
          Just req ->
            maybe True (`L.isInfixOf` req) msource &&
            maybe True (\ target -> ("<value><string>" ++ target ++ "</string></value>") `L.isInfixOf` req) mtarget

-- getTagID :: String -- ^ tag
--          -> IO TagID
-- getTagID tag =
--   TagId <$> koji "getTagID" tag

-- | Get the userid for the named user
kojiGetUserID :: String -- ^ hub url
              -> String -- ^ user
              -> IO (Maybe UserID)
kojiGetUserID hubUrl name = do
  res <- getUser hubUrl (InfoString name) False
  return $ readID =<< res

-- | Get the tags of a build
kojiBuildTags :: String  -- ^ hub url
              -> BuildInfo
              -> IO [String]
kojiBuildTags hubUrl buildinfo = do
  lst <- listTags hubUrl (Just (buildInfo buildinfo)) Nothing False
  return $ mapMaybe (lookupStruct "name") lst

-- | The state of a build
data BuildState = BuildBuilding
                | BuildComplete
                | BuildDeleted
                | BuildFailed
                | BuildCanceled
  deriving (Eq, Enum, Show)

readBuildState :: Value -> BuildState
readBuildState (ValueInt i) | i `elem` map fromEnum (enumFrom BuildBuilding) = toEnum i
readBuildState _ = error "invalid build state"

-- | Get the state of a build
kojiGetBuildState :: String -- ^ hub url
                  -> BuildInfo
                  -> IO (Maybe BuildState)
kojiGetBuildState hubUrl buildinfo =
  ((fmap readBuildState . lookupStruct "state") =<<) <$>
  getBuild hubUrl (buildInfo buildinfo)

-- | The state of a task
data TaskState = TaskFree
               | TaskOpen
               | TaskClosed
               | TaskCanceled
               | TaskAssigned
               | TaskFailed
  deriving (Eq, Enum, Show)

-- | Open task states
openTaskStates :: [TaskState]
openTaskStates = [TaskFree, TaskOpen, TaskAssigned]

openTaskValues :: Value
openTaskValues = ValueArray $ map taskStateToValue openTaskStates
  where
    taskStateToValue :: TaskState -> Value
    taskStateToValue = ValueInt . fromEnum

readTaskState :: Value -> TaskState
readTaskState (ValueInt i) | i `elem` map fromEnum (enumFrom TaskFree) = toEnum i
readTaskState _ = error "invalid task state"

getTaskState :: Struct -> Maybe TaskState
getTaskState st = readTaskState <$> lookup "state" st

-- | Get the state of a taskid
kojiGetTaskState :: String -- ^ hub url
                 -> TaskID
                 -> IO (Maybe TaskState)
kojiGetTaskState hubUrl tid = do
  mti <- getTaskInfo hubUrl (getID tid) False
  return $ case mti of
             Nothing -> Nothing
             Just ti -> readTaskState <$> lookupStruct "state" ti

-- | Get info about a task
kojiGetTaskInfo :: String -- ^ hub url
                -> TaskID
                -> IO (Maybe Struct)
kojiGetTaskInfo hubUrl tid = getTaskInfo hubUrl (getID tid) True
  -- res <- kojiCall "getTaskInfo" [show taskid]
  -- let state = res ^? key "state" % _Integer <&> (toEnum . fromInteger)
  --     arch = res ^? key "arch" % _String
  -- return $ TaskInfo arch state

-- | Get the children tasks of a task
kojiGetTaskChildren :: String -- ^ hub url
                    -> TaskID
                    -> Bool
                    -> IO [Struct]
kojiGetTaskChildren hubUrl tid =
  getTaskChildren hubUrl (getID tid)

-- | Get the latest build of a package in a tag
kojiLatestBuild :: String -- ^ hub
                -> String -- ^ tag
                -> String -- ^ pkg
                -> IO (Maybe Struct)
kojiLatestBuild hubUrl tag pkg =
  listToMaybe <$> getLatestBuilds hubUrl (InfoString tag) Nothing (Just pkg) Nothing

-- | Get latest build in a tag for package at a time event.
--
-- Used for example to implement waitrepo
kojiLatestBuildRepo :: String -- ^ hub
                    -> String -- ^ tag
                    -> Int    -- ^ event
                    -> String -- ^ pkg
                    -> IO (Maybe Struct)
kojiLatestBuildRepo hubUrl tag event pkg =
  listToMaybe <$> getLatestBuilds hubUrl (InfoString tag) (Just event) (Just pkg) Nothing

-- | Build metadata
data KojiBuild
  = KojiBuild
      { kbBuildId :: Int
      , kbPackageId :: Int
      , kbOwnerName :: String
      , kbNvr :: String
      }
  deriving (Show)

-- | List builds in a tag
kojiListTaggedBuilds :: String -- ^ hub url
                     -> Bool -- ^ latest
                     -> String -- ^ tag
                     -> IO [KojiBuild]
kojiListTaggedBuilds hubUrl latest tag =
  mapMaybe readKojiBuild <$> listTagged hubUrl tag Nothing False Nothing latest Nothing Nothing Nothing
  where
    readKojiBuild :: Struct -> Maybe KojiBuild
    readKojiBuild values = do
      buildId <- lookupStruct "build_id" values
      packageId <- lookupStruct "package_id" values
      owner <- lookupStruct "owner_name" values
      nvr <- lookupStruct "nvr" values
      return $ KojiBuild buildId packageId owner nvr

-- | Get the build and dest tags for a target.
kojiBuildTarget :: String -- ^ hubUrl
                -> String -- ^ target
                -> IO (Maybe (String, String)) -- ^ (build-tag,dest-tag)
kojiBuildTarget hub target = do
  mres <- maybeStruct <$> getBuildTarget hub target
  case mres of
    Nothing -> return Nothing
    Just res -> return $ readTarget res
  where
  readTarget res = do
    buildtag <- lookupStruct "build_tag_name" res
    desttag <- lookupStruct "dest_tag_name" res
    return (buildtag, desttag)

-- | List sidetags (preferably for user and/or basetag)
kojiListSideTags :: String -- ^ hubUrl
                 -> Maybe String -- ^ basetag
                 -> Maybe String -- ^ user
                 -> IO [String] -- ^ list of sidetags
kojiListSideTags hub mbasetag muser =
  mapMaybe (lookupStruct "name") . structArray <$> listSideTags hub (InfoString <$> mbasetag) (InfoString <$> muser)

-- | Repo state
data RepoState = RepoInit
               | RepoReady
               | RepoExpired
               | RepoDeleted
               | RepoProblem
  deriving (Eq, Enum, Show)

readRepoState :: Value -> RepoState
readRepoState (ValueInt i) | i `elem` map fromEnum (enumFrom RepoInit) = toEnum i
readRepoState _ = error "invalid repo state"

-- getRepoState :: Struct -> Maybe RepoState
-- getRepoState st = readRepoState <$> lookup "state" st

-- | Get repo info for tag
kojiGetRepo :: String -- ^ hub url
            -> String -- ^ tag
            -> Maybe RepoState
            -> Maybe Int -- ^ event
            -> IO (Maybe Struct) -- ^ result
kojiGetRepo hub tag mstate mevent =
  maybeStruct <$> getRepo hub tag (fromEnum <$> mstate) mevent False

-- | Get current repo info for tag
kojiGetCurrentRepo :: String -> String -> IO (Maybe Struct)
kojiGetCurrentRepo hub tag =
  maybeStruct <$> getRepo hub tag Nothing Nothing False
