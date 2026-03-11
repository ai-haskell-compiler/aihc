{-# LANGUAGE CPP, OverloadedStrings #-}

module Release (
  showReleaseCmd,
  getRelease,
  activeFedoraReleases,
  BodhiRelease(..),
  downloadServer
  )
where

import Control.Monad.Extra (forM_, unless, void, when, whenJust)
import Data.Bifunctor (first)
import qualified Data.CaseInsensitive as CI
import Data.List.Extra
import Data.Maybe (fromMaybe)
import Data.Time.Format (defaultTimeLocale, parseTimeM, rfc822DateFormat)
import Data.Time.LocalTime (utcToLocalZonedTime)
import Network.Curl (curlHead, CurlOption(..))
import Safe (tailSafe)
import SimpleCmd (error', (+-+))
import Text.Regex (mkRegex, subRegex)

import Arch
import BodhiRelease
import Common (warning)
import Types
import URL

showReleaseCmd :: Bool -> Bool -> RepoSource -> Release  -> Arch -> Maybe Arch
               -> Bool -> IO ()
showReleaseCmd debug dynredir reposource release sysarch march testing =
  void $ getRelease debug dynredir False True reposource release sysarch march testing

getRelease :: Bool -> Bool -> Bool -> Bool -> RepoSource
            -> Release -> Arch -> Maybe Arch -> Bool -> IO [(String, URL)]
getRelease debug dynredir warn checkdate reposource@(RepoSource koji _mirror) release sysarch march testing = do
  let arch = fromMaybe sysarch march
  (url,path) <- getURL debug dynredir reposource release arch
  let urlpath = url +//+ path
  when debug $ print $ renderUrl Dir urlpath
  (basicrepos,morerepos) <-
    if koji
    then return ([("koji",urlpath)],[])
    else
    case release of
      Centos n -> return ([("BaseOS",urlpath)],
                          [("AppStream",url),(if n >= 9 then "CRB" else "PowerTools",url)])
      ELN -> return ([("BaseOS",urlpath)],
                     [("AppStream",urlpath),("CRB",urlpath)])
      Rawhide -> return ([("development", urlpath)],[])
      -- curl -s https://bodhi.fedoraproject.org/releases/?exclude_archived=True | jq
      Fedora n -> do
        eactiverelease <- activeFedoraRelease n
        case eactiverelease of
          Left _oldest ->
              return (("releases", urlpath) :
                      ("updates", url +//+ ["updates",show n]) :
                      [("updates-testing", url +//+ ["updates","testing",show n]) | testing]
                     ,[])
          Right rel -> do
            let composed = releaseComposed rel
                state = releaseState rel
                postbeta = releasePostBeta rel
            return $
          -- after Beta Freeze
          -- {"state": "pending", "composed_by_bodhi": true,
          --  "create_automatic_updates": false, "setting_status": "post_beta"}
          --
          -- final freeze
          -- {"state": "frozen", "composed_by_bodhi": true,
          --  "create_automatic_updates": false, "setting_status": "post_beta"}
          --
          -- final unfrozen
          -- {"state": "current", "composed_by_bodhi": true,
          --  "create_automatic_updates": false, "setting_status": "post_beta"}
          --
          -- released
          -- {"state": "current", "composed_by_bodhi": true,
          --  "create_automatic_updates": false, "setting_status": null}
          --
          -- Rawhide
          -- {"state": "pending", "composed_by_bodhi": false,
          --  "create_automatic_updates": true, "setting_status": "pre_beta"}
              if state `elem` ["pending","frozen"] || state == "current" && postbeta
              then (("development", urlpath) :
                    [("updates", url +//+ ["updates",show n]) | postbeta , state == "current"] ++
                    -- FIXME add way to disable or invert testing
                    [("updates-testing", url +//+ ["updates","testing",show n]) | testing || composed, state /= "current"]
                   ,[])
              else (("releases", urlpath) :
                    ("updates", url +//+ ["updates",show n]) :
                    [("updates-testing", url +//+ ["updates","testing",show n]) | testing || postbeta]
                   ,[])
      EPEL n -> return
                (("epel",urlpath) :
                 [("epel-testing",url +//+ ["testing", show n]) | testing],
                 [])
      EPEL10Dot n -> return
                (("epel",urlpath) :
                 [("epel-testing",url +//+ ["testing", "10." ++ show n]) | testing],
                 [])
      EPELNext _n -> return ([("epelnext",urlpath)],[])
      System -> error' "showRelease: system unsupported"
  rawhide <- rawhideFedoraRelease
  let basicrepourls =
        map (repoConfigArgs reposource sysarch march rawhide release) basicrepos
      morerepourls =
        map (repoConfigArgs reposource sysarch march rawhide release) morerepos
  forM_ basicrepourls $ \(reponame,(url',path')) -> do
    let baserepo = url' +//+ path'
    when debug $ print $ renderUrl Dir baserepo
    unless (not checkdate || release == System) $ do
      let composeinfo =
            url' +//+
            if koji
            then ["repo.json"]
            else
              case release of
                Centos 10 -> ["metadata","composeinfo.json"]
                Centos _ -> ["COMPOSE_ID"] -- ["metadata","composeinfo.json"]
                ELN -> ["COMPOSE_ID"]
                EPEL _ -> ["Everything", "state"]
                EPEL10Dot _ -> ["Everything", "state"]
                EPELNext _ -> ["Everything", "state"]
                Fedora _ -> if "updates" `isInfixOf` reponame
                            then ["Everything", "state"]
                            else ["COMPOSE_ID"]
                Rawhide -> ["COMPOSE_ID"]
                System -> error' "system not supported"
      let composeUrl = renderUrl File composeinfo
      when debug $ print composeUrl
      mtimestr <- curlGetHeader Nothing "Last-Modified" composeUrl
      whenJust mtimestr $ \timestr -> do
        utc <- parseTimeM False defaultTimeLocale rfc822DateFormat timestr
        date <- utcToLocalZonedTime utc
        (if warn then warning else putStrLn) $ show date +-+ "<" ++ renderUrl Dir url' ++ ">"
  return $ map (fmap (uncurry (+//+))) $ basicrepourls ++ morerepourls

getURL :: Bool -> Bool -> RepoSource -> Release -> Arch
       -> IO (URL,[String])
getURL debug dynredir reposource@(RepoSource koji _mirror) release arch =
  case release of
    Centos n ->
      case n of
        10 ->
          let url = URL $
                if koji
                then "https://odcs.stream.centos.org/stream-10"
                else "https://composes.stream.centos.org/stream-10/production/latest-CentOS-Stream/compose/"
          in return (url,[])
        9 ->
          let url = URL $
                if koji
                then "https://odcs.stream.centos.org"
                else "https://mirror.stream.centos.org/9-stream/"
          in return (url,[])
        8 -> return (URL "http://vault.centos.org/8-stream/", [])
        _ -> error' "old Centos is not supported"
    ELN ->
      getFedoraServer debug dynredir reposource ["eln"] ["1"]
    EPEL n | n < 7 ->
               return
               (URL "https://archives.fedoraproject.org/pub/archive/epel", [show n])
    EPEL n -> getFedoraServer debug dynredir reposource ["epel"] [show n]
    EPEL10Dot n -> getFedoraServer debug dynredir reposource ["epel"] ["10." ++ show n]
    EPELNext n -> getFedoraServer debug dynredir reposource ["epel","next"] [show n]
    Fedora n -> do
      eactiverelease <- activeFedoraRelease n
      case eactiverelease of
        Left oldest ->
          if n < oldest
          then
          return
          (URL "https://archives.fedoraproject.org/pub/archive" +//+ fedoraTop, ["releases", show n])
          else error' $ "unknown fedora release:" +-+ show n
        Right rel ->
          -- state values: ["disabled","pending","frozen","current","archived"]
          let pending = releaseState rel /= "current"
              postbeta = releasePostBeta rel
              rawhide = pending && releaseBranch rel == "rawhide"
              releasestr = if rawhide then "rawhide" else show n
          in
            if arch == RISCV64
            then return (URL "http://fedora.riscv.rocks",[])
            else
              getFedoraServer debug dynredir reposource fedoraTop
              [if pending || postbeta then "development" else "releases",
               releasestr]
    Rawhide -> getFedoraServer debug dynredir reposource fedoraTop ["development", "rawhide"]
    System -> error' "getURL: system unsupported"
  where
    fedoraTop =
      -- FIXME support older archs
      if arch `elem` [PPC64LE, S390X]
      then ["fedora-secondary"]
      else ["fedora", "linux"]

repoConfigArgs :: RepoSource -> Arch -> Maybe Arch -> Natural -> Release
               -> (String,URL) -> (String,(URL,[String]))
repoConfigArgs (RepoSource False _mirror) sysarch (Just RISCV64) _rawhide release (repo,url) =
  let (compose,path) =
        case release of
          Rawhide -> (["repos-dist", show release, "latest"],"")
          _ -> (["repos-dist", show release, "latest"],"")
      arch = RISCV64
      reponame = repo ++ "-" ++ show release ++
                 if arch == sysarch then "" else "-" ++ showArch arch
  in (reponame, (url +//+ compose, [path, showArch arch, ""]))
-- non-koji
repoConfigArgs (RepoSource False mirror) sysarch march rawhide release (repo,url) =
  let arch = fromMaybe sysarch march
      archsuffix = if arch == sysarch then "" else "-" ++ showArch arch
      reponame = repoVersion ++ archsuffix ++
                 case mirror of
                   DownloadFpo -> ""
                   Mirror serv ->
                     '-' : takeWhile (/= '/') (subRegex (mkRegex "https?://") serv "")
                   DlFpo -> "-dl.fpo"
      path =
        case release of
          Centos _ -> [repo, showArch arch] ++ (if arch == Source then ["tree"] else ["os"])
          ELN -> [repo, showArch arch] ++ (if arch == Source then ["tree"] else ["os"])
          EPEL n -> (if n >= 8 then ("Everything" :) else id) [showArch arch] ++ ["tree" | arch == Source]
          EPEL10Dot _n -> "Everything" : showArch arch : ["tree" | arch == Source]
          EPELNext _n -> ["Everything", showArch arch]
          Fedora _ -> ["Everything", showArch arch] ++ (if arch == Source then ["tree"] else ["os" | repo `elem` ["releases","development"]])
          Rawhide -> ["Everything", showArch arch, if arch == Source then "tree" else "os"]
          System -> error' "repoConfigArgs: system unsupported"
  in (reponame, (url, path ++ ["/"]))
  where
    repoVersion :: String
    repoVersion =
      case release of
        Rawhide -> "fedora-rawhide"
        Fedora n ->
          if n == rawhide
          then "fedora-rawhide"
          else show release ++ if repo == "releases" then "" else '-':repo
        ELN -> show release ++ '-':repo
        EPEL10Dot _ -> show release ++ if repo == "epel-testing" then "-testing" else ""
        EPELNext _ -> show release ++ if repo == "epelnext-testing" then "-testing" else ""
        EPEL _ -> show release ++ if repo == "epel-testing" then "-testing" else ""
        Centos _ -> show release ++ '-':repo
        System -> ""
-- koji
repoConfigArgs (RepoSource True _mirror) sysarch march _rawhide release (repo,url) =
  let (compose,path) =
        case release of
          Rawhide -> (["repos", show release, "latest"],"")
          _ -> (["repos", show release ++ "-build/latest"],"")
      arch = fromMaybe sysarch march
      reponame = repo ++ "-" ++ show release ++ "-build" ++
                 if arch == sysarch then "" else "-" ++ showArch arch
  in (reponame, (url +//+ compose, [path, showArch arch, ""]))
-- repoConfigArgs url (RepoCentosStream chan) arch release repo =
--   let (compose,path) = (["composes", channel chan, "latest-CentOS-Stream", "compose"], repo)
--       reponame = repo ++ "-Centos-" ++ show release ++ "-Stream" ++ "-" ++ show chan ++ if arch == sysarch then "" else "-" ++ showArch arch
--   in (reponame, (url +//+ compose, [path, showArch arch, "os/"]))

getFedoraServer :: Bool -> Bool -> RepoSource -> [String] -> [String]
                -> IO (URL,[String])
getFedoraServer debug dynredir (RepoSource koji mirror) top path =
  if koji
  then return (URL "https://kojipkgs.fedoraproject.org",[])
  else
    case mirror of
      DownloadFpo -> do
        let url = URL downloadServer +//+ top ++ path
            rurl = renderUrl Dir url
        when debug $ print rurl
        redir <-
          if dynredir
          then return Nothing
          else curlGetHeader (Just 3000) "Location" rurl
        when debug $ print redir
        case redir of
          Nothing -> do
            when (debug && not dynredir) $
              warning $ "slow redirection for" +-+ rurl
            return (URL downloadServer +//+ top,path)
          Just actual -> do
            when debug $ do
              warning rurl
              warning $ "redirected to" +-+ show actual
            let actualstr =
                  if "https://ftp.yzyu.jp/" `isPrefixOf` actual
                  then replace "https://" "http://" actual
                  else actual
            when (actual /= actualstr) $
              warning $ "replacing to" +-+ actualstr
            return (URL $ removeSubpath path actualstr, path)
      -- FIXME how to handle any path
      Mirror serv -> return (URL serv,path)
      DlFpo -> return (URL "https://dl.fedoraproject.org/pub" +//+ top, path)

downloadServer :: String
downloadServer = "https://download.fedoraproject.org/pub"

-- FIXME check CurlCode(CurlOK) status
curlGetHeader :: Maybe Natural -> CI.CI String -> String -> IO (Maybe String)
curlGetHeader mtimeout field url = do
  (_status,headers) <- curlHead url $ maybe [] (singleton . CurlConnectTimeoutMS . fromIntegral) mtimeout
  -- tail because curl leaves space before field value
  return $ tailSafe <$> lookup field (map (first CI.mk) headers)

#if !MIN_VERSION_base(4,15,0)
singleton :: a -> [a]
singleton x = [x]
#endif
