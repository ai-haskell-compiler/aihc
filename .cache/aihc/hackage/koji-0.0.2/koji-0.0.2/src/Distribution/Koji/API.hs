{-|
Low level XMLRPC API based directly on the python API
since that is the only available reference documentation:
<https://koji.fedoraproject.org/koji/api>.

Note that many of the procedures in this module are untested and experimental:
particularly most of those that return a Value.

Higher level wrapper procedures live in Distribution.Koji.

Fixes and enhancements welcome.
-}



module Distribution.Koji.API
       ( Info(..)
       , koji
       , hello
       , checkTagAccess
       , checkTagPackage
       , getAPIVersion
       , getActiveRepos
       , getAllArches
       , getAllPerms
       , getArchive
       , getArchiveFile
       , getArchiveType
       , getArchiveTypes
       , getAverageBuildDuration
       , getBuild
       , getBuildConfig
       , getBuildLogs
       , getBuildTarget
       , getBuildTargets
       , getBuildType
       , getBuildroot
       , getBuildrootListing
       , getChangelogEntries
       , getChannel
       , getEvent
       , getExternalRepo
       , getExternalRepoList
       , getFullInheritance
       , getGlobalInheritance
       , getGroupMembers
       , getHost
       , getImageArchive
       , getImageBuild
       , getInheritanceData
       , getLastEvent
       , getLastHostUpdate
       , getLatestBuilds
       , getLatestMavenArchives
       , getLatestRPMS
       , getMavenArchive
       , getMavenBuild
       , getNextRelease
       , getPackage
       , getPackageConfig
       , getPackageID
       , getRPM
       , getRPMDeps
       , getRPMFile
       , getRPMHeaders
       , getRepo
       , getTag
       , getTagExternalRepos
       , getTagGroups
       , getTagID
       , getTaskChildren
       , getTaskDescendents
       , getTaskInfo
       , getTaskRequest
       , getTaskResult
       , getUser
       , getUserPerms
       , getVolume
       , getWinArchive
       , getWinBuild

       , listArchiveFiles
       , listArchives
       , listBTypes
       , listBuildRPMs
       , listBuildroots
       , listBuilds
       , listCGs
       , listChannels
       , listExternalRepos
       , listHosts
       , listPackages
       , listPackagesSimple
       , listRPMFiles
       , listRPMs
       , listSideTags
       , listTagged
       , listTaggedArchives
       , listTaggedRPMS
       , listTags
       , listTaskOutput
       , listTasks
       , listUsers
       , listVolumes

       , repoInfo
       , resubmitTask
       , tagChangedSinceEvent
       , tagHistory
       , taskFinished
       , taskReport

       , Value(..)
       , Struct
       , lookupStruct
--       , readStructString
--       , readStructArray
--       , readMethodParams
       , maybeStruct
       , structArray
       , getValue
       )
where

import Data.Maybe
--import Control.Monad.Fail (MonadFail)
--import qualified Data.ByteString.Lazy.Char8 as B
import Network.XmlRpc.Client
import Network.XmlRpc.Internals
--import Network.HTTP.Simple
--import Network.HTTP.Client.Conduit
import Control.Monad.Except (runExceptT)

-- | Low-level variadic XML RPC call to a Koji Hub
koji :: Remote a
     => String -- ^ kojihub url
     -> String -- ^ command
     -> a
koji = remote

-- xmlrpc :: String -> String -> [Value] -> IO Value
-- xmlrpc url m args = do
--   initreq <- parseRequest url
--   let reqbody = renderCall (MethodCall m args)
--       request = setRequestBody (RequestBodyLBS reqbody) $ setRequestMethod "POST" initreq
--   resp <- httpLBS request
--   let respbody = getResponseBody resp
--   handleError fail $ (parseResponse (B.unpack respbody) >>= handleResponse)
--   where
--     -- | Gets the return value from a method response.
--     --   Throws an exception if the response was a fault.
--     handleResponse :: MonadFail m => MethodResponse -> m Value
--     handleResponse (Return v)       = return v
--     handleResponse (Fault code str) = error ("Error " ++ show code ++ ": " ++ str)

-- FIXME rename to newtype KojiStruct?
type Struct = [(String,Value)]

maybeString :: Maybe String -> Value
maybeString = maybe ValueNil ValueString

maybeInt :: Maybe Int -> Value
maybeInt = maybe ValueNil ValueInt

maybeValue :: Maybe Value -> Value
maybeValue = fromMaybe ValueNil

-- | info is either an id or string
data Info = InfoID Int | InfoString String

infoValue :: Info -> Value
infoValue (InfoID i) = ValueInt i
infoValue (InfoString s) = ValueString s

maybeInfo :: Maybe Info -> Value
maybeInfo = maybe ValueNil infoValue

maybeStruct :: Value -> Maybe Struct
maybeStruct (ValueStruct st) = Just st
maybeStruct _ = Nothing

structArray :: Value -> [Struct]
structArray (ValueArray v) = mapMaybe maybeStruct v
structArray _ = []

-- https://koji.fedoraproject.org/koji/api

-- CG*

-- _listapi()

-- add*

-- applyVolumePolicy(build, strict=False)

-- assignTask(task_id, host, force=False)

-- build(src, target, opts=None, priority=None, channel=None)

-- buildContainer(src, target, opts=None, priority=None, channel='container')

-- buildImage(name, version, arch, target, ksfile, img_type, opts=None, priority=None)

-- buildImageIndirection(opts=None, priority=None)

-- buildImageOz(name, version, arches, target, inst_tree, opts=None, priority=None)

-- buildReferences(build, limit=None, lazy=False)

-- cancelBuild(buildID)

-- cancelTask(task_id, recurse=True)

-- cancelTaskChildren(task_id)

-- cancelTaskFull(task_id, strict=True)

-- chainBuild(srcs, target, opts=None, priority=None, channel=None)

-- chainMaven(builds, target, opts=None, priority=None, channel='maven')

-- changeBuildVolume(build, volume, strict=True)

-- | checkTagAccess(tag_id, user_id=None)
checkTagAccess :: String -> Int -> Int -> IO Value
checkTagAccess hubUrl = koji hubUrl "checkTagAccess"

-- | checkTagPackage(tag, pkg)
checkTagPackage :: String -> Info -> Info -> IO Bool
checkTagPackage hubUrl taginfo pkginfo = koji hubUrl "checkTagPackage" (infoValue taginfo) (infoValue pkginfo)

-- count*

-- create*

-- debugFunction(name, *args, **kwargs)

-- delete*

-- disable*

-- distRepo(tag, keys, **task_opts)

-- downloadTaskOutput(taskID, fileName, offset=0, size=-1, volume=None)

-- dropGroupMember(group, user)

-- echo(*args)

-- edit*

-- enable*

-- error

-- filterResults(methodName, *args, **kw)

-- findBuildID(X, strict=False)

-- freeTask(task_id)

-- | getAPIVersion()
getAPIVersion :: String -> IO String
getAPIVersion hubUrl = koji hubUrl "getAPIVersion"

-- | getActiveRepos()
getActiveRepos :: String -> IO Value
getActiveRepos hubUrl = koji hubUrl "getActiveRepos"

-- | getAllArches
getAllArches :: String -> IO Value
getAllArches hubUrl = koji hubUrl "getAllArches"

-- | getAllPerms
getAllPerms :: String -> IO [Struct]
getAllPerms hubUrl = koji hubUrl "getAllPerms"

-- | getArchive(archive_id, strict=False)
getArchive :: String -> Int -> IO (Maybe Struct)
getArchive hubUrl = fmap maybeStruct . koji hubUrl "getArchive"

-- | getArchiveFile(archive_id, filename, strict=False)
getArchiveFile :: String -> Int -> FilePath -> IO (Maybe Struct)
getArchiveFile hubUrl archiveID file = maybeStruct <$> koji hubUrl "getArchiveFile" archiveID file

-- | getArchiveType(filename=None, type_name=None, type_id=None, strict=False)
getArchiveType :: String -> Maybe FilePath -> Maybe String -> Maybe Int -> IO Value
getArchiveType hubUrl filename type_name type_id =
  koji hubUrl "getArchiveType" (maybeString filename) (maybeString type_name) (maybeInt type_id)

-- | getArchiveTypes()
getArchiveTypes :: String -> IO Value
getArchiveTypes hubUrl = koji hubUrl "getArchiveTypes"

-- | getAverageBuildDuration pkginfo
getAverageBuildDuration :: String -> Info -> IO Value
getAverageBuildDuration hubUrl = koji hubUrl "getAverageBuildDuration" . infoValue

-- | getBuild(buildInfo, strict=False)
getBuild :: String
         -> Info -- ^ buildID
         -> IO (Maybe Struct)
getBuild hubUrl = fmap maybeStruct . koji hubUrl "getBuild" . infoValue

-- | getBuildConfig tag
getBuildConfig :: String -> String -> IO Value
getBuildConfig hubUrl = koji hubUrl "getBuildConfig"

-- | getBuildLogs build
getBuildLogs :: String -> Info -- ^ buildinfo
             -> IO Value
getBuildLogs hubUrl = koji hubUrl "getBuildLogs" . infoValue

-- | getBuildTarget info
getBuildTarget :: String -> String -> IO Value
getBuildTarget hubUrl = koji hubUrl "getBuildTarget"

-- | getBuildTargets info event buildTagID destTagID
getBuildTargets :: String -> Maybe Info -> Maybe Int -> Maybe Int -> Maybe Int -> IO Value
getBuildTargets hubUrl info event buildTagId destTagId  =
  koji hubUrl "getBuildTargets" (maybeInfo info) (maybeInt event) (maybeInt buildTagId) (maybeInt destTagId)

-- | getBuildType buildinfo
getBuildType :: String -> Info -- ^ buildinfo
             -> IO Value
getBuildType hubUrl = koji hubUrl "getBuildType" . infoValue

-- | getBuildroot buildrootId
getBuildroot :: String -> Int -> IO Value
getBuildroot hubUrl = koji hubUrl "getBuildroot"

-- | getBuildrootListing buildrootId
getBuildrootListing :: String -> Int -> IO Value
getBuildrootListing hubUrl = koji hubUrl "getBuildrootListing"

-- | getChangelogEntries(buildID=None, taskID=None, filepath=None, author=None, before=None, after=None, queryOpts=None)
getChangelogEntries :: String
                    -> Maybe Int -- ^ buildID
                    -> Maybe Int -- ^ taskID
                    -> Maybe FilePath
                    -> Maybe String -- ^ author
                    -> Maybe String -- ^ before
                    -> Maybe String -- ^ after
                    -> IO [Struct]
getChangelogEntries hubUrl buildID taskID filepath author before after =
  koji hubUrl "getChangelogEntries" (maybeInt buildID) (maybeInt taskID) (maybeString filepath) (maybeString author) (maybeString before) (maybeString after)

-- | getChannel channelinfo
getChannel :: String -> Info -> IO Value
getChannel hubUrl = koji hubUrl "getChannel" . infoValue

-- | getEvent eventid
getEvent :: String -> Int -> IO Struct
getEvent hubUrl = koji hubUrl "getEvent"

-- | getExternalRepo info
getExternalRepo :: String -> Info -> Maybe Int -> IO Struct
getExternalRepo hubUrl info event =
  koji hubUrl "getExternalRepo" (infoValue info) () (maybeInt event)

-- | getExternalRepoList(tag_info, event=None)
getExternalRepoList :: String -> Info -> Maybe Int -> IO [Struct]
getExternalRepoList hubUrl info event =
  koji hubUrl "getExternalRepoList" (infoValue info) (maybeInt event)

-- | getFullInheritance(tag, event=None, reverse=False, stops=None, jumps=None)
getFullInheritance :: String -> String -> Maybe Int -> Bool -> IO Value
getFullInheritance hubUrl tag event =
  koji hubUrl "getFullInheritance" tag (maybeInt event)

-- | getGlobalInheritance(event=None)
getGlobalInheritance :: String -> Maybe Int -> IO Value
getGlobalInheritance hubUrl = koji hubUrl "getGlobalInheritance" . maybeInt

-- | getGroupMembers(group)
getGroupMembers :: String -> String -> IO Value
getGroupMembers hubUrl = koji hubUrl "getGroupMembers"

-- | getHost(hostInfo, strict=False, event=None)
getHost :: String -> Info -> Maybe Int -> IO Struct
getHost hubUrl info = koji hubUrl "getHost" (infoValue info) () . maybeInt

-- | getImageArchive(archive_id, strict=False)
getImageArchive :: String -> Int -> IO Struct
getImageArchive hubUrl = koji hubUrl "getImageArchive"

-- | getImageBuild(buildInfo, strict=False)
getImageBuild :: String -> Info -> IO Struct
getImageBuild hubUrl info = koji hubUrl "getImageBuild" (infoValue info)

-- | getInheritanceData(tag, event=None)
getInheritanceData :: String -> String -> Maybe Int -> IO Value
getInheritanceData hubUrl tag event =
  koji hubUrl "getInheritanceData" tag (maybeInt event)


-- | getLastEvent(before=None)
getLastEvent :: String -> Maybe Int -> IO Value
getLastEvent hubUrl = koji hubUrl "getLastEvent" . maybeInt

-- | getLastHostUpdate(hostID)
getLastHostUpdate :: String -> Int -> IO Value
getLastHostUpdate hubUrl = koji hubUrl "getLastHostUpdate"

-- | getLatestBuilds(tag, event=None, package=None, type=None)
--
-- List latest builds for tag (inheritance enabled)
getLatestBuilds :: String
                -> Info -- ^ tag
                -> Maybe Int -- ^ event
                -> Maybe String -- ^ pkg
                -> Maybe String -- ^ type
                -> IO [Struct]
getLatestBuilds hubUrl tag event pkg type_ =
  koji hubUrl "getLatestBuilds" (infoValue tag) (maybeInt event) (maybeString pkg) (maybeString type_)

-- | getLatestMavenArchives(tag, event=None, inherit=True)
getLatestMavenArchives :: String -> String -> Maybe Int -> Bool -> IO Value
getLatestMavenArchives hubUrl tag event =
  koji hubUrl "getLatestMavenArchives" tag (maybeInt event)

-- | getLatestRPMS(tag, package=None, arch=None, event=None, rpmsigs=False, type=None)
getLatestRPMS :: String -> String -> Maybe String -> Maybe String -> Maybe Int -> Bool -> Maybe String -> IO Value
getLatestRPMS hubUrl tag pkg arch event rpmsigs type_ =
  koji hubUrl "getLatestRPMS" tag (maybeString pkg) (maybeString arch) (maybeInt event) rpmsigs (maybeString type_)

-- getLoggedInUser()

-- | getMavenArchive(archive_id, strict=False)
getMavenArchive :: String -> Int -> IO Struct
getMavenArchive hubUrl = koji hubUrl "getMavenArchive"

-- | getMavenBuild(buildInfo, strict=False)
getMavenBuild :: String -> Info -> IO Struct
getMavenBuild hubUrl info = koji hubUrl "getMavenBuild" (infoValue info)

-- | getNextRelease(build_info)
--
-- find the last successful or deleted build of this N-V.
-- If building is specified, skip also builds in progress
getNextRelease :: String -> Info -> IO Value
getNextRelease hubUrl info = koji hubUrl "getNextRelease" (infoValue info)

-- | getPackage(info, strict=False, create=False)
--
-- Get the id,name for package
getPackage :: String -> Info -> IO Value
getPackage hubUrl info = koji hubUrl "getPackage" (infoValue info)

-- | getPackageConfig(tag, pkg, event=None)
--
-- Get config for package in tag
getPackageConfig :: String -> String -> String -> Maybe Int -> IO Value
getPackageConfig hubUrl tag pkg event =
  koji hubUrl "getPackageConfig" tag pkg (maybeInt event)

-- | getPackageID(name, strict=False)
--
-- Get package ID by name.
getPackageID :: String -> String -> IO (Maybe Int)
getPackageID hubUrl pkg = do
  res <- koji hubUrl "getPackageID" pkg
  case res of
    ValueInt i -> return $ Just i
    _ -> return Nothing

-- | getRPM(rpminfo, strict=False, multi=False)
getRPM :: String -> Info -> IO Struct
getRPM hubUrl = koji hubUrl "getRPM" . infoValue

-- | getRPMDeps(rpmID, depType=None, queryOpts=None, strict=False)
getRPMDeps :: String -> Int -> Maybe String -> IO [Struct]
getRPMDeps hubUrl rpmid deptype =
  koji hubUrl "getRPMDeps" rpmid (maybeString deptype)

-- | getRPMFile(rpmID, filename, strict=False)
getRPMFile :: String -> Int -> FilePath -> IO Struct
getRPMFile hubUrl = koji hubUrl "getRPMFile"

-- | getRPMHeaders(rpmID=None, taskID=None, filepath=None, headers=None)
getRPMHeaders :: String -> Maybe Int -> Maybe Int -> Maybe FilePath -> Maybe Value -> IO Struct
getRPMHeaders hubUrl rpmid taskid file headers =
  koji hubUrl "getRPMHeaders" (maybeInt rpmid) (maybeInt taskid) (maybeString file) (fromMaybe ValueNil headers)

-- | getRepo(tag, state=None, event=None, dist=False)
getRepo :: String -> String -> Maybe Int -> Maybe Int -> Bool -> IO Value
getRepo hubUrl tag state event =
  koji hubUrl "getRepo" tag (maybeInt state) (maybeInt event)

-- getSessionInfo()

-- | getTag(tagInfo, strict=False, event=None)
getTag :: String -> Info -> Maybe Int -> IO Struct
getTag hubUrl info = koji hubUrl "getTag" (infoValue info) () . maybeInt

-- | getTagExternalRepos(tag_info=None, repo_info=None, event=None)
getTagExternalRepos :: String -> Maybe Info -> Maybe Info -> Maybe Int -> IO Struct
getTagExternalRepos hubUrl taginfo repoinfo event =
  koji hubUrl "getTagExternalRepos" (maybeInfo taginfo) (maybeInfo repoinfo) (maybeInt event)

-- | getTagGroups(tag, event=None, inherit=True, incl_pkgs=True, incl_reqs=True, incl_blocked=False)
getTagGroups :: String -> String -> Maybe Int -> Bool -> Bool -> Bool -> Bool -> IO Value
getTagGroups hubUrl tag event =
  koji hubUrl "getTagGroups" tag (maybeInt event)

-- | getTagID(info, strict=False, create=False)
getTagID :: String -> Info -> IO (Maybe Int)
getTagID hubUrl tag = do
  res <- koji hubUrl "getTagID" (infoValue tag)
  case res of
    ValueInt i -> return $ Just i
    _ -> return Nothing

-- | getTaskChildren(task_id, request=False, strict=False)
getTaskChildren :: String -> Int -> Bool -> IO [Struct]
getTaskChildren hubUrl = koji hubUrl "getTaskChildren"

-- | getTaskDescendents(task_id, request=False)
getTaskDescendents :: String -> Int -> Bool -> IO Struct
getTaskDescendents hubUrl = koji hubUrl "getTaskDescendents"

-- | getTaskInfo(task_id, request=False, strict=False)
getTaskInfo :: String
            -> Int
            -> Bool -- ^ include request details
            -> IO (Maybe Struct)
getTaskInfo hubUrl tid request = maybeStruct <$> koji hubUrl "getTaskInfo" tid request
  -- res <- kojiCall "getTaskInfo" [show taskid]
  -- let state = res ^? key "state" % _Integer <&> (toEnum . fromInteger)
  --     arch = res ^? key "arch" % _String
  -- return $ TaskInfo arch state

-- | getTaskRequest(taskId)
getTaskRequest :: String -> Int -> IO Value
getTaskRequest hubUrl = koji hubUrl "getTaskRequest"

-- | getTaskResult(taskId, raise_fault=True)
getTaskResult :: String -> Int -> IO Value
getTaskResult hubUrl = koji hubUrl "getTaskResult"

-- | getUser(userInfo=None, strict=False, krb_princs=True)
getUser :: String -> Info -> Bool -> IO (Maybe Struct)
getUser hubUrl info krbprncpl =
  maybeStruct <$> koji hubUrl "getUser" (infoValue info) () krbprncpl

-- | getUserPerms(userID=None)
getUserPerms :: String -> Maybe Info -> IO Value
getUserPerms hubUrl = koji hubUrl "getUserPerms" . maybeInfo

-- | getVolume(volume, strict=False)
getVolume :: String -> Info -> IO Value
getVolume hubUrl = koji hubUrl "getVolume" . infoValue

-- | getWinArchive(archive_id, strict=False)
getWinArchive :: String -> Int -> IO Struct
getWinArchive hubUrl = koji hubUrl "getWinArchive"

-- | getWinBuild(buildInfo, strict=False)
getWinBuild :: String -> Info -> IO (Maybe Struct)
getWinBuild hubUrl = fmap maybeStruct . koji hubUrl "getWinBuild" . infoValue

-- grantCGAccess(user, cg, create=False)

-- grantPermission(userinfo, permission, create=False)

-- groupListAdd(taginfo, grpinfo, block=False, force=False, **opts)

-- groupListBlock(taginfo, grpinfo)

-- groupListRemove(taginfo, grpinfo, force=False)

-- groupListUnblock(taginfo, grpinfo)

-- groupPackageListAdd(taginfo, grpinfo, pkg_name, block=False, force=False, **opts)

-- groupPackageListBlock(taginfo, grpinfo, pkg_name)

-- groupPackageListRemove(taginfo, grpinfo, pkg_name, force=False)

-- groupReqListUnblock(taginfo, grpinfo, reqinfo)

-- hasPerm(perm, strict=False)

-- | Say hello to test XMLRPC connection to Koji Hub
hello :: String -> IO String
hello hubUrl = koji hubUrl "hello"

-- host.* [skipped]

-- importArchive(filepath, buildinfo, type, typeInfo)

-- importRPM(path, basename)

-- krbLogin(*args, **opts)

-- | listArchiveFiles(archive_id, queryOpts=None, strict=False)
listArchiveFiles :: String -> Int -> IO [Struct]
listArchiveFiles hubUrl = koji hubUrl "listArchiveFiles"

-- | listArchives(buildID=None, buildrootID=None, componentBuildrootID=None, hostID=None, type=None, filename=None, size=None, checksum=None, typeInfo=None, queryOpts=None, imageID=None, archiveID=None, strict=False)
listArchives :: String -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe String -> Maybe FilePath -> Maybe Int -> Maybe String -> Maybe Info -> Maybe Int -> Maybe Int -> IO [Struct]
listArchives hubUrl buildID buildrootID componentBuildrootID hostID type_ file size checksum typeInfo imageID archiveID =
  koji hubUrl "listArchives" (maybeInt buildID) (maybeInt buildrootID) (maybeInt componentBuildrootID) (maybeInt hostID) (maybeString type_) (maybeString file) (maybeInt size) (maybeString checksum) (maybeInfo typeInfo) () (maybeInt imageID) (maybeInt archiveID)

-- | listBTypes(query=None, queryOpts=None)
listBTypes :: String -> Value -> IO Value
listBTypes hubUrl = koji hubUrl "listBTypes"

-- | listBuildRPMs(build)
listBuildRPMs :: String -> Int -> IO [Struct]
listBuildRPMs hubUrl = koji hubUrl "listBuildRPMs"

-- | listBuildroots(hostID=None, tagID=None, state=None, rpmID=None, archiveID=None, taskID=None, buildrootID=None, queryOpts=None)
listBuildroots :: String -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> IO Value
listBuildroots hubUrl hostID tagID state rpmID archiveID taskID buildrootID =
  koji hubUrl "listBuildroots" (maybeInt hostID) (maybeInt tagID) (maybeInt state) (maybeInt rpmID) (maybeInt archiveID) (maybeInt taskID) (maybeInt buildrootID)

-- | listBuilds(packageID=None, userID=None, taskID=None, prefix=None, state=None, volumeID=None, source=None, createdBefore=None, createdAfter=None, completeBefore=None, completeAfter=None, type=None, typeInfo=None, queryOpts=None, pattern=None)
--
-- Note that pattern is only supported for koji >= 1.24
listBuilds :: String -> Struct -> IO [Struct]
listBuilds hubUrl args =
  let maybeArg fld = maybeValue (lookupStruct fld args)
  -- koji 1.24 added support for pattern
  in case maybeArg "pattern" of
       ValueNil ->
         koji hubUrl "listBuilds" (maybeArg "packageID") (maybeArg "userID") (maybeArg "taskID") (maybeArg "prefix") (maybeArg "state") (maybeArg "volumeID") (maybeArg "source") (maybeArg "createdBefore") (maybeArg "createdAfter") (maybeArg "completeBefore") (maybeArg "completeAfter") (maybeArg "type") (maybeArg "typeInfo") (maybeArg "queryOpts")
       pat -> koji hubUrl "listBuilds" (maybeArg "packageID") (maybeArg "userID") (maybeArg "taskID") (maybeArg "prefix") (maybeArg "state") (maybeArg "volumeID") (maybeArg "source") (maybeArg "createdBefore") (maybeArg "createdAfter") (maybeArg "completeBefore") (maybeArg "completeAfter") (maybeArg "type") (maybeArg "typeInfo") (maybeArg "queryOpts") pat

-- | listCGs()
listCGs :: String -> IO Struct
listCGs hubUrl = koji hubUrl "listCGs"

-- | listChannels(hostID=None, event=None)
listChannels :: String -> Maybe Int -> Maybe Int -> IO Value
listChannels hubUrl hostID event =
  koji hubUrl "listChannels" (maybeInt hostID) (maybeInt event)

-- | listExternalRepos(info=None, url=None, event=None, queryOpts=None)
listExternalRepos :: String -> Maybe Info -> Maybe String -> Maybe Int -> IO Value
listExternalRepos hubUrl info url event =
  koji hubUrl "listExternalRepos" (maybeInfo info) (maybeString url) (maybeInt event)

-- | listHosts(arches=None, channelID=None, ready=None, enabled=None, userID=None, queryOpts=None)
listHosts :: String -> Maybe Value -> Maybe Int -> Bool -> Bool -> Maybe Int -> IO Value
listHosts hubUrl arches channelID ready enabled userID =
  koji hubUrl "listHosts" (maybeValue arches) (maybeInt channelID) ready enabled (maybeInt userID)

-- | listPackages(tagID=None, userID=None, pkgID=None, prefix=None, inherited=False, with_dups=False, event=None, queryOpts=None)
listPackages :: String -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe String -> Bool -> Bool -> Maybe Int -> IO [Struct]
listPackages hubUrl tagID userID pkgID prefix inherited with_dups event =
  koji hubUrl "listPackages" (maybeInt tagID) (maybeInt userID) (maybeInt pkgID) (maybeString prefix) inherited with_dups (maybeInt event)

-- | listPackagesSimple prefix
listPackagesSimple :: String -> String -- ^ package name search prefix
                   -> IO [Struct]
listPackagesSimple hubUrl = koji hubUrl "listPackagesSimple"

-- | listRPMFiles(rpmID, queryOpts=None)
listRPMFiles :: String -> Int -> IO [Struct]
listRPMFiles hubUrl = koji hubUrl "listRPMFiles"

-- | listRPMs(buildID=None, buildrootID=None, imageID=None, componentBuildrootID=None, hostID=None, arches=None, queryOpts=None)
listRPMs :: String -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Value -> IO [Struct]
listRPMs hubUrl buildID buildrootID imageID componentBuildrootID hostID arches =
  koji hubUrl "listRPMs" (maybeInt buildID) (maybeInt buildrootID) (maybeInt imageID) (maybeInt componentBuildrootID) (maybeInt hostID) (maybeValue arches)

-- | listSideTags(basetag=None, user=None, queryOpts=None)
listSideTags :: String -> Maybe Info -> Maybe Info -> IO Value
listSideTags hubUrl basetag user = do
  koji hubUrl "listSideTags" (maybeInfo basetag) (maybeInfo user)

-- | listTagged(tag, event=None, inherit=False, prefix=None, latest=False, package=None, owner=None, type=None)
listTagged :: String -> String -> Maybe Int -> Bool -> Maybe String -> Bool -> Maybe String -> Maybe String -> Maybe String -> IO [Struct]
listTagged hubUrl tag event inherit prefix latest package owner type_ =
  koji hubUrl "listTagged" tag (maybeInt event) inherit (maybeString prefix) latest (maybeString package) (maybeString owner) (maybeString type_)

-- | listTaggedArchives(tag, event=None, inherit=False, latest=False, package=None, type=None)
listTaggedArchives :: String -> String -> Maybe Int -> Bool -> Bool -> Maybe String -> Maybe String -> IO Value
listTaggedArchives hubUrl tag event inherit latest package type_ =
  koji hubUrl "listTaggedArchives" tag (maybeInt event) inherit latest (maybeString package) (maybeString type_)

-- | listTaggedRPMS(tag, event=None, inherit=False, latest=False, package=None, arch=None, rpmsigs=False, owner=None, type=None)
listTaggedRPMS :: String -> String -> Maybe Int -> Bool -> Bool -> Maybe String ->  Maybe String -> Bool -> Maybe String -> Maybe String -> IO Value
listTaggedRPMS hubUrl tag event inherit latest package arch rpmsigs owner type_ =
  koji hubUrl "listTaggedRPMS" tag (maybeInt event) inherit latest (maybeString package) (maybeString arch) rpmsigs (maybeString owner) (maybeString type_)

-- | listTags(build=None, package=None, perms=True, queryOpts=None)
listTags :: String -> Maybe Info -> Maybe Info -> Bool -> IO [Struct]
listTags hubUrl build package =
  koji hubUrl "listTags" (maybeInfo build) (maybeInfo package)

-- | listTaskOutput(taskID, stat=False, all_volumes=False, strict=False)
listTaskOutput :: String -> Int -> Bool -> Bool -> Bool -> IO Struct
listTaskOutput hubUrl = koji hubUrl "listTaskOutput"

-- | listTasks(opts=None, queryOpts=None)
listTasks :: String -> Struct -- ^ opts
          -> Struct -- ^ qopts
          -> IO [Struct]
listTasks hubUrl = koji hubUrl "listTasks"

-- | listUsers(userType=0, prefix=None, queryOpts=None)
listUsers :: String -> Maybe Int -> Maybe String -> IO [Struct]
listUsers hubUrl userType prefix =
  koji hubUrl "listUsers" (maybeInt userType) (maybeString prefix)

-- | listVolumes()
listVolumes :: String -> IO Value
listVolumes hubUrl = koji hubUrl "listVolumes"

-- login(*args, **opts)

-- logout()

-- logoutChild(session_id)

-- makeTask(*args, **opts)

-- mavenBuild(url, target, opts=None, priority=None, channel='maven')

-- mavenEnabled()

-- mergeScratch(task_id)

-- moveAllBuilds(tag1, tag2, package, force=False)

-- moveBuild(tag1, tag2, build, force=False)

-- newGroup(name)

-- newRepo(tag, event=None, src=False, debuginfo=False, separate_src=False)

-- packageListAdd(taginfo, pkginfo, owner=None, block=None, extra_arches=None, force=False, update=False)

-- packageListBlock(taginfo, pkginfo, force=False)

-- packageListRemove(taginfo, pkginfo, force=False)

-- packageListSetArches(taginfo, pkginfo, arches, force=False)

-- packageListSetOwner(taginfo, pkginfo, owner, force=False)

-- packageListUnblock(taginfo, pkginfo, force=False)

-- queryHistory(tables=None, **kwargs)

-- queryRPMSigs(rpm_id=None, sigkey=None, queryOpts=None)

-- remove*

-- repo*

-- | repoInfo(repo_id, strict=False)
repoInfo :: String -> Int -> IO Value
repoInfo hubUrl = koji hubUrl "repoInfo"

-- resetBuild(build)

-- restartHosts(priority=5, options=None)

-- | resubmitTask(taskID)
resubmitTask :: String -> Int -> IO Value
resubmitTask hubUrl = koji hubUrl "resubmitTask"

-- revoke*

-- runroot(tagInfo, arch, command, channel=None, **opts)

-- search(terms, type, matchType, queryOpts=None)

-- set*

-- sharedSession()

-- showOpts()

-- showSession()

-- sslLogin(*args, **opts)

-- subsession()

-- system.*

-- tagBuild(tag, build, force=False, fromtag=None)

-- tagBuildBypass(tag, build, force=False, notify=True)

-- | tagChangedSinceEvent(event, taglist)
tagChangedSinceEvent :: String -> Int -> Value -> IO Bool
tagChangedSinceEvent hubUrl = koji hubUrl "tagChangedSinceEvent"

-- | tagHistory(build=None, tag=None, package=None, active=None, queryOpts=None)
tagHistory :: String -> Maybe Info -> Maybe Info -> Maybe Info -> Bool -> IO Value
tagHistory hubUrl build tag package =
  koji hubUrl "tagHistory" (maybeInfo build) (maybeInfo tag) (maybeInfo package)

-- | taskFinished(taskId)
taskFinished :: String -> Int -> IO Bool
taskFinished hubUrl = koji hubUrl "taskFinished"

-- | taskReport(owner=None)
taskReport :: String -> Maybe String -> IO Value
taskReport hubUrl = koji hubUrl "taskReport" . maybeString

-- untag*

-- updateNotification(id, package_id, tag_id, success_only)

-- uploadFile(path, name, size, md5sum, offset, data, volume=None)

-- winBuild(vm, url, target, opts=None, priority=None, channel='vm')

-- winEnabled()

-- wrapperRPM(build, url, target, priority=None, channel='maven', opts=None)

-- writeSignedRPM(an_rpm, sigkey, force=False)


-- readStructString :: String -> Struct -> String
-- readStructString key str =
--   case lookup key str of
--     Just (ValueString s) -> s
--     _ -> error $ "No String for " ++ key

-- readStructArray :: String -> Struct -> Maybe [Value]
-- readStructArray key struct =
--   either error id <$> runExceptT (getField key struct)
-- --    Just (ValueArray s) -> s
-- --    _ -> error $ "No Array for " ++ key

-- readMethodParams :: String -> Maybe [Value]
-- readMethodParams s =
--   either error params <$> runExceptT (parseCall s)
--   where
--     params (MethodCall _m str) = str

-- | Lookup a key in a XML result
lookupStruct :: XmlRpcType a => String -> Struct -> Maybe a
lookupStruct key struct =
  either error id <$> runExceptT (getField key struct)

getValue :: XmlRpcType a => Value -> Maybe a
getValue = fmap (either error id) . runExceptT . fromValue
