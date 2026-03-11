{-# LANGUAGE CPP #-}

-- see https://pagure.io/pungi-fedora/

#if !MIN_VERSION_base(4,13,0)
import Control.Applicative ((<|>)
#if !MIN_VERSION_base(4,8,0)
  , (<$>), (<*>)
#endif
  )
import Data.Semigroup ((<>))
#endif

import Control.Exception.Extra (retry)
import Control.Monad.Extra (filterM, unless, unlessM, when, whenJust, whenM)
import qualified Data.ByteString.Char8 as B
import Data.Char (isAlphaNum, isDigit)
import Data.List.Extra
import Data.Ord (comparing, Down(Down))
import Data.Maybe
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Time.LocalTime (getCurrentTimeZone, utcToZonedTime, TimeZone)
import Distribution.Fedora.Release (getCurrentFedoraVersion, getRawhideVersion)
import Network.HTTP.Client (managerResponseTimeout, newManager,
                            responseTimeoutNone)
import Network.HTTP.Client.TLS
import Network.HTTP.Directory
import Numeric.Natural

import Options.Applicative (fullDesc, header, progDescDoc)

import Paths_dl_fedora (version)

import SimpleCmd (cmd, cmd_, cmdBool, error', grep_, logMsg,
                  pipe_, pipeBool, pipeFile_,
                  warning, (+-+),
#if MIN_VERSION_simple_cmd(0,2,7)
                  sudoLog
#else
                  sudo_
#endif
                  )
import SimpleCmdArgs hiding (str)
import SimplePrompt (yesNo, yesNoDefault)
import System.Console.Pretty (color, Color(Red,Yellow))
import System.Directory (createDirectory, doesDirectoryExist, doesFileExist,
                         findExecutable, getHomeDirectory, getPermissions,
                         listDirectory, pathIsSymbolicLink, removeFile,
                         renameFile, setCurrentDirectory, withCurrentDirectory,
                         writable)
import System.FilePath (dropFileName, isAbsolute, isRelative, makeRelative,
                        joinPath, takeExtension, takeFileName, (</>), (<.>))
import System.Posix.Files (createSymbolicLink, fileSize, getFileStatus,
                           readSymbolicLink)
import System.Posix.User (getLoginName)

import Text.Read
import qualified Text.ParserCombinators.ReadP as R
import qualified Text.ParserCombinators.ReadPrec as RP
import Text.Regex.Posix
import qualified Text.PrettyPrint.ANSI.Leijen as P

import DownloadDir

data FedoraEdition = Cloud
                   | Container
                   | Everything
                   | KDE
                   | Server
                   | Workstation
                   | Budgie  -- first spin: used below
                   | Cinnamon
                   | COSMIC
                   | I3
                   | KDEMobile
                   | LXDE
                   | LXQt
                   | MATE
                   | Miracle
                   | SoaS
                   | Sway
                   | Xfce  -- last spin: used below
                   | Silverblue
                   | Kinoite
                   | Onyx
                   | Sericea
                   | IoT
 deriving (Show, Enum, Bounded, Eq)

showEdition :: Release -> FedoraEdition -> String
showEdition rel KDE =
  if fedoraVerOrLater 42 rel
  then "KDE-Desktop"
  else "KDE"
showEdition _ KDEMobile = "KDE-Mobile"
showEdition _ MATE = "MATE_Compiz"
showEdition _ Miracle = "MiracleWM"
showEdition _ I3 = "i3"
showEdition _ e = show e

showShortEdition :: Release -> FedoraEdition -> String
showShortEdition rel edition =
  if edition == KDE then "KDE" else showEdition rel edition

lowerEdition :: FedoraEdition -> String
lowerEdition = lower . show

instance Read FedoraEdition where
  readPrec = do
    s <- look
    let e = lower s
        editionMap =
          ("gnome", Workstation) :
          ("sb", Silverblue) :
          ("ws", Workstation) :
          map (\ ed -> (lowerEdition ed, ed)) [minBound..maxBound]
        res = lookup e editionMap
    case res of
      Nothing -> error' ("unknown edition: " ++ show s) >> RP.pfail
      Just ed -> RP.lift (R.string e) >> return ed

type URL = String

fedoraSpins :: Release -> [FedoraEdition]
fedoraSpins rel =
  (if fedoraVerOrLater 42 rel then id else (KDE :)) [Budgie .. Xfce]

fedoraVerOrLater :: Natural -> Release -> Bool
fedoraVerOrLater _ Rawhide = True
fedoraVerOrLater n (Fedora m) = m >= n
fedoraVerOrLater _ FedoraStage = True
fedoraVerOrLater _ FedoraTest = True
fedoraVerOrLater _ _ = False

allSpins :: Natural -> Natural -> Release -> [FedoraEdition]
allSpins rawhide current rel =
  case rel of
    Rawhide -> allSpins rawhide current $ Fedora rawhide
    Fedora r -> fedoraSpins rel \\ missingEditions r
    FedoraRespin -> delete KDEMobile $ allSpins rawhide current $ Fedora current
    FedoraTest -> allSpins rawhide current $ Fedora current -- FIXME use fedora-releases
    FedoraStage -> allSpins rawhide current $ Fedora (current + 1) -- FIXME use fedora-releases
    CS 9 True -> [Cinnamon, KDE, MATE, Xfce] -- FIXME missing MAX, MIN
    CS 10 True -> [KDE] -- FIXME missing MAX, MIN
    CS _ False -> []
    CS _ _ -> []
    ELN -> []

allEditions :: Natural -> Natural -> Release -> [FedoraEdition]
allEditions rawhide current rel =
  case rel of
    Rawhide -> allEditions rawhide current $ Fedora rawhide
    Fedora r -> [minBound..maxBound] \\ missingEditions r
    FedoraRespin -> Workstation : allSpins rawhide current FedoraRespin
    FedoraTest -> allEditions rawhide current $ Fedora current -- FIXME use fedora-releases
    FedoraStage -> allEditions rawhide current $ Fedora (current + 1) -- FIXME use fedora-releases
    CS _ True -> Workstation : allSpins rawhide current rel
    CS _ False -> [Workstation]
    _ -> error' "--all-editions not supported for this release"

missingEditions :: Natural -> [FedoraEdition]
missingEditions r =
  case compare r 41 of
    GT -> [IoT]
    EQ -> [COSMIC]
    LT -> [COSMIC, KDEMobile, Miracle]

data RequestEditions = Editions Bool [FedoraEdition]
                     | AllDesktops
                     | AllSpins
                     | AllEditions
  deriving Eq

data CheckSum = AutoCheckSum | NoCheckSum | CheckSum
  deriving Eq

dlFpo, downloadFpo, kojiPkgs, odcsFpo, csComposes, odcsStream, csMirror
  :: String
dlFpo = "https://dl.fedoraproject.org/pub" -- main repo
downloadFpo = "https://download.fedoraproject.org/pub" -- mirror redirect
kojiPkgs = "https://kojipkgs.fedoraproject.org/compose"
odcsFpo = "https://odcs.fedoraproject.org/composes"
csComposes = "https://composes.stream.centos.org"
odcsStream = "https://odcs.stream.centos.org" -- no cdn
csMirror = "https://mirror.stream.centos.org"

data Mirror = DlFpo | UseMirror | KojiFpo | Mirror String | DefaultLatest
  deriving Eq

data CentosChannel = CSProduction | CSTest | CSDevelopment
  deriving Eq

showChannel :: CentosChannel -> String
showChannel CSProduction = "production"
showChannel CSTest = "test"
showChannel CSDevelopment = "development"

data Release = Fedora Natural
             | FedoraRespin
             | Rawhide
             | FedoraTest
             | FedoraStage
             | ELN
             | CS Natural Bool -- alt live respin
  deriving Eq

readRelease :: Natural -> Natural -> String -> Release
readRelease rawhide current rel =
  case lower rel of
    'f': n@(_:_) | all isDigit n -> Fedora (read n)
    n@(_:_) | all isDigit n ->
              let v = read n in
                case compare v 11 of
                  LT -> CS v False
                  EQ -> ELN
                  GT ->
                    case compare v rawhide of
                      LT -> Fedora v
                      EQ -> Rawhide
                      GT -> error' $ "Current rawhide is" +-+ show rawhide
    "next" -> Fedora (current + 1)
    "current" -> Fedora current
    "previous" -> Fedora (current -1)
    -- FIXME hardcoding
    "c8s" -> CS 8 False
    "c9s" -> CS 9 False
    "c10s" -> CS 10 False
    "9-live" -> CS 9 True
    "9-respin" -> CS 9 True
    "c9s-live" -> CS 9 True
    "c9s-respin" -> CS 9 True
    "10-live" -> CS 10 True
    "10-respin" -> CS 10 True
    "c10s-live" -> CS 10 True
    "c10s-respin" -> CS 10 True
    "respin" -> FedoraRespin
    "rawhide" -> Rawhide
    "test" -> FedoraTest
    "stage" -> FedoraStage
    "eln" -> ELN
    _ -> error' "unknown release"

data Mode = Check | Local | List | Download Bool -- replace
  deriving Eq

main :: IO ()
main = do
  let pdoc = Just $ P.vcat
             [ P.text "Tool for downloading Fedora iso file images.",
               P.text ("RELEASE = " <> intercalate ", " ["release number", "respin", "rawhide", "test (Beta)", "stage (RC)", "eln", "c9s", "c10s", "c9s-live"]),
               P.text "EDITION = " <> P.lbrace <> P.align (P.fillCat (P.punctuate P.comma (map (P.text . lowerEdition) [(minBound :: FedoraEdition)..maxBound])) <> P.rbrace) <> P.text " [default: workstation]" ,
               P.text "",
               P.text "See <https://github.com/juhp/dl-fedora/#readme>"
             ]
  sysarch <- readArch <$> cmd "rpm" ["--eval", "%{_arch}"]
  rawhideVersion <- getRawhideVersion
  currentRelease <- getCurrentFedoraVersion
  home <- getHomeDirectory
  defaultDir <- checkDefaultIsoDir
  simpleCmdArgsWithMods (Just version) (fullDesc <> header "Fedora iso downloader" <> progDescDoc pdoc) $
    program rawhideVersion
    <$> switchWith 'g' "gpg-keys" "Import Fedora GPG keys for verifying checksum file"
    <*> checksumOpts
    <*> switchLongWith "debug" "Debug output"
    <*> switchWith 'T' "no-http-timeout" "Do not timeout for http response"
    <*> (flagWith' Check 'c' "check" "Check if newer image available" <|>
         flagWith' Local 'l' "local" "Show current local image" <|>
         flagLongWith' List "list" "List spins and editions" <|>
         Download <$> switchWith 'R' "replace" "Delete previous snapshot image after downloading latest one")
    <*> strOptionalWith 'd' "dir" "DIRECTORY" ("Download directory [default:" +-+ prettyDir home defaultDir ++ "]") defaultDir
    <*> switchWith 'n' "dry-run" "Don't actually download anything"
    <*> switchWith 'r' "run" "Boot image in QEMU"
    <*> mirrorOpt
    <*> switchLongWith "dvd" "Download dvd iso instead of boot netinst (for Server, eln, centos)"
    <*> optional (flagLongWith' CSDevelopment "cs-devel" "Use centos-stream development compose" <|>
                  flagLongWith' CSTest "cs-test" "Use centos-stream test compose" <|>
                  flagLongWith' CSProduction "cs-production" "Use centos-stream production compose (default is mirror.stream.centos.org)")
    <*> optional (strOptionLongWith "alt-cs-extra-edition" "('MAX'|'MIN')" "Centos Stream Alternative Live Spin editions (MAX,MIN)")
    <*> (optionWith (eitherReader eitherArch) 'a' "arch" "ARCH" ("Specify arch [default:" +-+ showArch sysarch ++ "]") <|> pure sysarch)
    <*> (readRelease rawhideVersion currentRelease <$> strArg "RELEASE")
    <*> (flagLongWith' AllDesktops "all-desktops" "Get all Fedora desktops" <|>
         flagLongWith' AllSpins "all-spins" "Get all Fedora Spins" <|>
         flagLongWith' AllEditions "all-editions" "Get all Fedora editions" <|>
         (Editions
           <$> switchWith 'x' "exclude" "Exclude specified editions"
           <*> many (argumentWith auto "EDITION...")))
  where
    mirrorOpt :: Parser Mirror
    mirrorOpt =
      flagWith' DefaultLatest 'L' "latest" "Get latest image either from mirror or dl.fp.o if newer" <|>
      flagLongWith' DlFpo "dl" "Use dl.fedoraproject.org (dl.fp.o)" <|>
      flagWith' KojiFpo 'k' "koji" "Use koji.fedoraproject.org" <|>
      Mirror <$> strOptionWith 'm' "mirror" "URL" ("Mirror url for /pub [default " ++ downloadFpo ++ "]") <|>
      pure UseMirror

    checksumOpts :: Parser CheckSum
    checksumOpts =
      flagLongWith' NoCheckSum "no-checksum" "Do not check checksum" <|>
      flagLongWith AutoCheckSum CheckSum "checksum" "Do checksum even if already downloaded"

data Primary = Primary {primaryUrl :: String,
                        primarySize :: Maybe Integer,
                        primaryTime :: Maybe UTCTime}

program :: Natural -> Bool -> CheckSum -> Bool -> Bool -> Mode -> FilePath
        -> Bool -> Bool -> Mirror -> Bool -> Maybe CentosChannel
        -> Maybe String -> Arch -> Release -> RequestEditions -> IO ()
program rawhide gpg checksum debug notimeout mode dlDir dryrun run mirror dvdnet mchannel mcsedition arch tgtrel reqeditions = do
  when (isJust mchannel && not (isCentosStream tgtrel)) $
    error' "channels are only for centos-stream"
  let mirrorUrl =
        case mirror of
          Mirror m -> m
          KojiFpo -> kojiPkgs
          DlFpo -> dlFpo
          -- UseMirror or DefaultLatest
          _ ->
            case tgtrel of
              ELN -> odcsFpo
              CS n _ | n >= 9 -> if isJust mchannel
                               then csComposes
                               else csMirror
              CS 8 _ -> csComposes
              _ -> downloadFpo
  unlessM (doesDirectoryExist dlDir) $
    error' $ "No such directory:" +-+ dlDir
  setCurrentDirectory dlDir
  showdestdir <- do  -- only used for output to user
    home <- getHomeDirectory
    return $ prettyDir home dlDir

  when debug $ print showdestdir
  mgr <- if notimeout
         then newManager (tlsManagerSettings {managerResponseTimeout = responseTimeoutNone})
         else httpManager
  unless (reqeditions == Editions False []) $
    case tgtrel of
      ELN -> error' "cannot specify edition for eln"
      CS n False -> error' $ "cannot specify edition for centos-stream" ++
                    if n >= 9 then ": perhaps you want 'c" ++ show n ++ "s-live'?" else ""
      _ -> return ()
  when (isJust mcsedition) $
    case tgtrel of
      CS n True | n >= 9 ->
                  case reqeditions of
                    Editions False [] -> return ()
                    _ -> error' "combining extra edition unsupported"
      _ -> error' "--alt-cs-extra-edition is only for CS Alt Live spins"
  current <- getCurrentFedoraVersion
  successes <-
    mapM (runProgramEdition mgr mirrorUrl showdestdir gpg checksum debug mode dryrun run mirror dvdnet mchannel mcsedition arch tgtrel reqeditions) $
    if mode == List
    then [Workstation]
    else
      case reqeditions of
        AllDesktops -> Workstation : allSpins rawhide current tgtrel
        AllEditions -> allEditions rawhide current tgtrel
        AllSpins -> allSpins rawhide current tgtrel
        Editions False editions ->
          if null editions then [Workstation] else editions
        Editions True [] -> error' "cannot exclude zero editions"
        Editions True editions ->
           allEditions rawhide current tgtrel \\ editions
  let failures = length $ filter not successes
  when (failures > 0) $ error' $ plural failures "download" +-+ "failed"

prettyDir :: FilePath -> FilePath -> FilePath
prettyDir home dir =
  let path = makeRelative home dir
  in if isAbsolute dir && isRelative path then "~" </> path else dir

-- from fbrnch Common
plural :: Int -> String -> String
plural i ns =
  pluralException i Nothing ns (ns ++ "s")

pluralException :: Int -> Maybe String -> String -> String -> String
pluralException 0 (Just z) _ _ = z
pluralException i _ ns ps =
  mconcat
  [
    if i == 0 then "no" else show i,
    " ",
    if i == 1 then ns else ps
  ]

multipleEditions :: RequestEditions -> Bool
multipleEditions req =
  case req of
    Editions _ es -> length es > 1
    _ -> True

runProgramEdition :: Manager -> URL -> String -> Bool -> CheckSum -> Bool
                  -> Mode -> Bool -> Bool -> Mirror -> Bool
                  -> Maybe CentosChannel -> Maybe String -> Arch -> Release
                  -> RequestEditions -> FedoraEdition -> IO Bool
runProgramEdition mgr mirrorUrl showdestdir gpg checksum debug mode dryrun run mirror dvdnet mchannel mcsedition arch tgtrel reqeditions edition =
  case mode of
    Check -> do
      eres <- findURL True
      case eres of
        Left err -> do
          (if multipleEditions reqeditions then warning else error')  err
          return False
        Right (fileurl, filenamePrefix, _prime, _mchecksum, done) -> do
          if done
            then do
            putStrLn $ "Local:" +-+ takeFileName fileurl +-+ "is latest"
            when debug $ print fileurl
            else do
            let symlink = filenamePrefix <> (if tgtrel == ELN then "-" <> showArch arch else "") <> "-latest" <.> takeExtension fileurl
            mtarget <- derefSymlink symlink
            case mtarget of
              Just target -> do
                if target == takeFileName fileurl
                  then putStrLn $ "Latest:" +-+ target +-+ "(locally incomplete)"
                  else do
                  putStrLn $ "Local:" +-+ target
                  putStrLn $ "Newer:" +-+ takeFileName fileurl
              Nothing -> putStrLn $ "Available:" +-+ takeFileName fileurl
          return True
    Local -> do
      prefix <- getFilePrefix
        -- FIXME support non-iso
      let symlink = prefix <> (if tgtrel == ELN then "-" <> showArch arch else "") <> "-latest" <.> "iso"
      mtarget <- derefSymlink symlink
      whenJust mtarget $ \target -> do
        putStrLn $ "Local:" +-+ target
        when run $
          bootImage dryrun target showdestdir
      return True
    List -> do
      rawhide <- getRawhideVersion
      current <- getCurrentFedoraVersion
      let editions =
            case reqeditions of
              AllDesktops -> Workstation : allSpins rawhide current tgtrel
              AllSpins -> allSpins rawhide current tgtrel
              _ -> allEditions rawhide current tgtrel
          extras =
            case tgtrel of
              CS _ True -> ["max","min"]
              _ -> []
      putStrLn $ unwords $ sort (map lowerEdition editions) ++ extras
      return True
    Download removeold -> do
      eres <- findURL False
      case eres of
        Left err -> do
          (if multipleEditions reqeditions then warning else error')  err
          return False
        Right (fileurl, filenamePrefix, prime, mchecksum, done) -> do
          let symlink = filenamePrefix <> (if tgtrel == ELN then "-" <> showArch arch else "") <> "-latest" <.> takeExtension fileurl
          downloadFile dryrun debug done mgr fileurl prime showdestdir >>=
            fileChecksum fileurl mchecksum
          unless dryrun $ do
            let localfile = takeFileName fileurl
            updateSymlink localfile symlink removeold
            when run $ bootImage dryrun localfile showdestdir
          return True
  where
    findURL :: Bool
            -- urlpath, fileprefix, primary, checksum, downloaded
            -> IO (Either String (URL, String, Primary, Maybe String, Bool))
    findURL quiet = do
      (path,mrelease) <- urlPathMRel
      let path' = trailingSlash path
      (primeDir,hrefs) <-
        case tgtrel of
          -- "koji" -> getUrlDirectory kojiPkgs path'
          ELN -> getUrlDirectory odcsFpo path'
          CS n _ ->
            if n < 9 || isJust mchannel
            then getUrlDirectory odcsStream path'
            else getUrlDirectory csMirror path'
          _ ->
            if mirror `elem` [DefaultLatest, DlFpo]
            then getUrlDirectory dlFpo path'
            else do
              (url,ls) <- getUrlDirectory downloadFpo path'
              if null ls
                then getUrlDirectory dlFpo path'
                else return (url,ls)
      when (null hrefs) $ error' $ primeDir +-+ "is empty"
      let (prefixPat,selector) = makeFileSelector mrelease
          fileslen = groupSortOn length $ filter selector $ map T.unpack hrefs
          mchecksum = find ((if tgtrel == FedoraRespin then T.isPrefixOf else T.isSuffixOf) (T.pack "CHECKSUM")) hrefs
      case fileslen of
        [] -> return $ Left $ "**" +-+ color Red "no match" +-+ "for" +-+ color Red prefixPat +-+ "in" +-+ primeDir
        (files:_) -> do
          when (length files > 1) $ mapM_ putStrLn files
          let file = last files
              prefix = if '[' `elem` prefixPat
                       then (file =~ prefixPat) ++ if showArch arch `isInfixOf` prefixPat then "" else showArch arch
                       else prefixPat
              primeUrl = primeDir +/+ file
          (primeSize,primeTime) <- retry 3 $ httpFileSizeTime mgr primeUrl
          (finalurl, already) <- do
            let localfile = takeFileName primeUrl
            exists <- doesFileExist localfile
            if exists
              then do
              localsize <- toInteger . fileSize <$> getFileStatus localfile
              done <- checkLocalFileSize localsize localfile primeSize primeTime quiet
              if done
                then return (primeUrl,True)
                else do
                unlessM (writable <$> getPermissions localfile) $ do
                  putStrLn $ localfile <> " does not have write permission!"
                  yes <- yesNo "Fix ownership"
                  if yes
                    then do
                    user <- getLoginName
                    sudoLog "chown" [user,localfile]
                    else
                    error' $ localfile <> " does not have write permission, aborting!"
                findMirror primeUrl path file
              else findMirror primeUrl path file
          let finalDir = dropFileName finalurl
          return $ Right
            (finalurl, prefix, Primary primeUrl primeSize primeTime,
             (finalDir +/+) . T.unpack <$> mchecksum, already)
        where
          getUrlDirectory :: String -> FilePath -> IO (String, [T.Text])
          getUrlDirectory top path = do
            let url = top +/+ path
            when debug $ do
              print url
              redirs <- retry 3 $ httpRedirects mgr $ replace "https:" "http:" url
              print redirs
            ls <- retry 3 $ httpDirectory mgr url
            return (url, ls)

          findMirror primeUrl path file = do
            url <-
              if mirrorUrl `elem` [dlFpo,kojiPkgs,odcsFpo]
              then return primeUrl
              else
                if mirrorUrl /= downloadFpo
                then return $ mirrorUrl +/+ path +/+ file
                else do
                  redir <- retry 3 $ httpRedirect mgr $ mirrorUrl +/+ path +/+ file
                  case redir of
                    Nothing -> do
                      warning $ mirrorUrl +/+ path +/+ file <> " redirect failed"
                      return primeUrl
                    Just u -> do
                      let url = B.unpack u
                      exists <- retry 3 $ httpExists mgr url
                      when debug $ print (url, exists)
                      if exists then return url
                        else return primeUrl
            return (url,False)

    getFilePrefix :: IO String
    getFilePrefix = do
      let (prefixPat,selector) = makeFileSelector getRelease
      symlinks <- listDirectory "." >>= filterM pathIsSymbolicLink
      return $
        case find selector (reverseSort symlinks) of
          Nothing ->
            if '[' `elem` prefixPat
            then error' $ "no match for " <> prefixPat <> " in " <> showdestdir
            else prefixPat
          Just symlink ->
            if '[' `elem` prefixPat
            then (symlink =~ prefixPat) ++ if showArch arch `isInfixOf` prefixPat then "" else showArch arch
            else prefixPat

    getRelease :: Maybe String
    getRelease =
      case tgtrel of
        -- FIXME IoT uses version for instead of Rawhide
        Rawhide -> Just "Rawhide"
        FedoraRespin -> Nothing
        ELN -> Nothing
        CS _ _ -> Nothing
        Fedora rel -> Just $ show rel
        _ -> error' "release target is unsupported with --dryrun"

    checkLocalFileSize :: Integer -> FilePath -> Maybe Integer -> Maybe UTCTime
                       -> Bool -> IO Bool
    checkLocalFileSize localsize localfile mprimeSize mprimeTime quiet = do
      if Just localsize == mprimeSize
        then do
        unless quiet $ do
          tz <- getCurrentTimeZone
          -- FIXME abbreviate size?
          putStrLn $ unwords [showdestdir </> localfile, renderTime tz mprimeTime, "size " ++ show localsize ++ " okay"]
        return True
        else do
        when (isNothing mprimeSize) $
          putStrLn "original size could not be read"
        let sizepercent =
              case mprimeSize of
                Nothing -> show localsize
                Just ms -> show (100 * localsize `div` ms) <> "%"
        putStrLn $ "File " <> sizepercent <> " downloaded"
        return False

    urlPathMRel :: IO (FilePath, Maybe String)
    urlPathMRel = do
      let subdir =
            if edition `elem` fedoraSpins tgtrel
            then joinPath ["Spins", showArch arch, "iso"]
            else joinPath [showShortEdition tgtrel edition, showArch arch, editionMedia edition]
      case tgtrel of
        FedoraRespin -> return ("alt/live-respins", Nothing)
        Rawhide -> return ("fedora/linux/development/rawhide" +/+ subdir, Just "Rawhide")
        FedoraTest -> testRelease subdir
        FedoraStage -> stageRelease subdir
        ELN -> return ("production/latest-Fedora-ELN/compose" +/+ "BaseOS" +/+ showArch arch +/+ "iso", Nothing)
        CS 8 False ->
          return ("stream-8" +/+ showChannel (fromMaybe CSProduction mchannel) +/+ "latest-CentOS-Stream/compose" +/+ "BaseOS" +/+ showArch arch +/+ "iso", Nothing)
        CS n cslive ->
          return $
          case mchannel of
            Nothing ->
              if cslive
              then ("SIGs" +/+ show n ++ "-stream/altimages/images/live" +/+ showArch arch, Nothing)
              else (show n ++ "-stream" +/+ "BaseOS" +/+ showArch arch +/+ "iso", Nothing)
            Just channel ->
              ((if n == 9 then id else (("stream-" ++ show n) +/+)) $ showChannel channel +/+ "latest-CentOS-Stream/compose" +/+ "BaseOS" +/+ showArch arch +/+ "iso", Nothing)
        Fedora n ->
          if edition == IoT
          then return ("alt/iot" +/+ show n +/+ subdir, Nothing)
          else released (show n) subdir

    testRelease :: FilePath -> IO (FilePath, Maybe String)
    testRelease subdir = do
      let path = "fedora/linux" +/+ "releases/test"
          url = dlFpo +/+ path
      rels <- map (T.unpack . noTrailingSlash) <$> retry 3 (httpDirectory mgr url)
      let mrel = if null rels then Nothing else Just (last rels)
      return (path +/+ fromMaybe (error' ("test release not found in " <> url)) mrel +/+ subdir, mrel)

    stageRelease :: FilePath -> IO (FilePath, Maybe String)
    stageRelease subdir = do
      let path = "alt/stage"
          url = dlFpo +/+ path
      rels <- reverse . map (T.unpack . noTrailingSlash) <$> retry 3 (httpDirectory mgr url)
      let mrel = listToMaybe rels
      return (path +/+ fromMaybe (error' ("staged release not found in " <> url)) mrel +/+ subdir, takeWhile (/= '_') <$> mrel)

    -- kojiCompose :: Manager -> FilePath -> IO (FilePath, Maybe String)
    -- kojiCompose mgr subdir = do
    --   let path = "branched"
    --       url = kojiPkgs +/+ path
    --       prefix = "latest-Fedora-"
    --   latest <- filter (prefix `isPrefixOf`) . map T.unpack <$> retry 3 $ httpDirectory mgr url
    --   let mlatest = listToMaybe latest
    --   return (path +/+ fromMaybe (error' ("koji branched latest dir not found in " <> url)) mlatest +/+ "compose" +/+ subdir, removePrefix prefix <$> mlatest)

    -- use https://admin.fedoraproject.org/pkgdb/api/collections ?
    released :: FilePath -> FilePath -> IO (FilePath, Maybe String)
    released rel subdir = do
      let dir = "fedora/linux/releases"
          url = dlFpo +/+ dir
      exists <- retry 3 $ httpExists mgr $ url +/+ rel
      when debug $ print (exists, url +/+ rel)
      if exists
        then return (dir +/+ rel +/+ subdir, Just rel)
        else do
        let dir' = "fedora/linux/development"
            url' = dlFpo +/+ dir'
        exists' <- retry 3 $ httpExists mgr $ url' +/+ rel
        if exists'
          then return (dir' +/+ rel +/+ subdir, Just rel)
          else error' "release not found in releases/ or development/"

    renderdvdboot = if dvdnet then "dvd1" else "boot"

    makeFileSelector :: Maybe String -> (String, String -> Bool)
    makeFileSelector mrelease =
      let (prefixPat,msuffix) = makeFilePrefixSuffix mrelease
          selector f =
            not ("-latest-" `isInfixOf` f) &&
            case msuffix of
              Just suffix -> f =~ (prefixPat ++ ".*-" ++ suffix)
              Nothing ->
                if '[' `elem` prefixPat
                then f =~ prefixPat
                else prefixPat `isPrefixOf` f
      in (prefixPat, selector)

    makeFilePrefixSuffix :: Maybe String -> (String, Maybe String)
    makeFilePrefixSuffix mrelease =
      case tgtrel of
        FedoraRespin ->
          ("F[1-9][0-9]-" <> liveRespin edition <> "-x86_64" <> "-LIVE",
           Nothing)
        ELN -> ("Fedora-eln", Just renderdvdboot)
        CS n cslive ->
          if cslive
          then ("CentOS-Stream-Image-" ++ maybe (csLive edition) upper mcsedition ++ "-Live" <.> showArch arch ++ '-' : show n, Nothing)
          else ("CentOS-Stream-" ++ show n, Just renderdvdboot)
        _ ->
          let showRel r = if last r == '/' then init r else r
              rel = maybeToList (showRel <$> mrelease)
              (midpref,middle) =
                case edition of
                  -- https://github.com/fedora-iot/iot-distro/issues/1
                  IoT -> ("", rel)
                  Cloud | tgtrel == Fedora 40 -> ('.' : showArch arch, rel)
                  Container | tgtrel == Fedora 40 -> ('.' : showArch arch, rel)
                  _ -> ("",
                        if edition `elem` kiwiSpins
                        then rel
                        else showArch arch : rel)
          in (intercalate "-" (["Fedora", showEdition tgtrel edition, editionType edition ++ midpref] ++ middle),
              Nothing)

    -- https://pagure.io/pungi-fedora/blob/main/f/fedora.conf#_251 kiwibuild
    kiwiSpins :: [FedoraEdition]
    kiwiSpins =
      -- https://fedoraproject.org/wiki/Changes/EROFSforLiveMedia
      (if fedoraVerOrLater 42 tgtrel
       then [Budgie, COSMIC, KDE, LXQt, SoaS, Workstation, Xfce]
       else []) ++
      (if fedoraVerOrLater 41 tgtrel
       then [Cloud, Container, KDEMobile, Miracle]
       else [])

    editionType :: FedoraEdition -> String
    editionType Server = if dvdnet then "dvd" else "netinst"
    editionType IoT = "ostree"
    editionType Kinoite = "ostree"
    editionType Onyx = "ostree"
    editionType Sericea = "ostree"
    editionType Silverblue = "ostree"
    editionType Everything = "netinst"
    editionType Cloud = "Base-Generic"
    editionType Container = "Base-Generic"
    editionType _ = "Live"

    fileChecksum :: String -> Maybe URL -> Maybe Bool -> IO ()
    fileChecksum _ Nothing _ = return ()
    fileChecksum imageurl (Just url) mneedChecksum =
      when ((mneedChecksum == Just True && checksum /= NoCheckSum) || (isJust mneedChecksum && checksum == CheckSum)) $ do
        let checksumdir = ".dl-fedora-checksums"
            checksumfilename = takeFileName url
            checksumpath = checksumdir </> checksumfilename
        exists <- do
          dirExists <- doesDirectoryExist checksumdir
          if dirExists
            then checkChecksumfile url checksumpath
            else createDirectory checksumdir >> return False
        putStrLn ""
        haveChksum <-
          if exists
          then return True
          else do
            remoteExists <- retry 3 $ httpExists mgr url
            if remoteExists
              then do
              withCurrentDirectory checksumdir $ do
                ok <- curl debug $ (if debug then ["--silent", "--show-error"] else []) ++ ["--remote-name", url]
                unless ok $ error' $ "failed to download" +-+ url
              doesFileExist checksumpath
              else return False
        if not haveChksum
          then putStrLn "No checksum file found"
          else do
          pgp <- grep_ "PGP" checksumpath
          when (gpg && pgp) $ do
            case tgtrel of
              Fedora n -> do
                havekey <- checkForFedoraKeys n
                unless havekey $ do
                  putStrLn "Importing Fedora GPG keys:\n"
                  -- https://fedoramagazine.org/verify-fedora-iso-file/
                  pipe_ ("curl",["--silent", "--show-error", "https://fedoraproject.org/fedora.gpg"]) ("gpg",["--import"])
                  putStrLn ""
              _ -> return ()
          chkgpg <-
            if pgp
            then
              case tgtrel of
                Fedora n -> checkForFedoraKeys n
                _ -> return False
            else return False
          let shasum = if "CHECKSUM512" `isPrefixOf` checksumfilename
                       then "sha512sum" else "sha256sum"
          if chkgpg then do
            putStrLn $ "Running gpg verify and " <> shasum <> ":"
            pipeFile_ checksumpath ("gpg",["-q"]) (shasum, ["-c", "--ignore-missing"])
            else do
            putStrLn $ "Running " <> shasum <> ":"
            -- FIXME ignore other downloaded iso's (eg partial images error)
            pipe_
              ("grep",[takeFileName imageurl,checksumpath])
              (shasum,["-c","-"])

    checkChecksumfile :: URL -> FilePath -> IO Bool
    checkChecksumfile url checksumfile = do
      exists <- doesFileExist checksumfile
      if not exists then return False
        else do
        filesize <- toInteger . fileSize <$> getFileStatus checksumfile
        when (filesize == 0) $ error' $ checksumfile +-+ "empty!"
        (primeSize,primeTime) <- retry 3 $ httpFileSizeTime mgr url
        when debug $ print (primeSize,primeTime,url)
        ok <- checkLocalFileSize filesize checksumfile primeSize primeTime True
        unless ok $ error' $ "Checksum file filesize mismatch for " ++ checksumfile
        return ok

    checkForFedoraKeys :: Natural -> IO Bool
    checkForFedoraKeys n =
      pipeBool ("gpg",["--list-keys"]) ("grep", ["-q", " Fedora .*(" <> show n <> ").*@fedoraproject.org>"])

    updateSymlink :: FilePath -> FilePath -> Bool -> IO ()
    updateSymlink target symlink removeold = do
      mmsymlinkTarget <- do
        havefile <- doesFileExist symlink
        if havefile
          then Just . Just <$> readSymbolicLink symlink
          else do
          -- check for broken symlink
          dirfiles <- listDirectory "."
          return $ if symlink `elem` dirfiles then Just Nothing else Nothing
      case mmsymlinkTarget of
        Nothing -> makeSymlink
        Just Nothing -> do
          removeFile symlink
          makeSymlink
        Just (Just symlinktarget) -> do
          when (symlinktarget /= target) $ do
            when removeold $ removeFile symlinktarget
            removeFile symlink
            makeSymlink
      where
        makeSymlink = do
          putStrLn ""
          createSymbolicLink target symlink
          putStrLn $ unwords [showdestdir </> symlink, "->", target]

    derefSymlink :: FilePath -> IO (Maybe FilePath)
    derefSymlink symlink = do
      havefile <- doesFileExist symlink
      if havefile
        then Just <$> readSymbolicLink symlink
        else return Nothing

renderTime :: TimeZone -> Maybe UTCTime -> String
renderTime tz mprimeTime =
  "(" ++ maybe "" (show . utcToZonedTime tz) mprimeTime ++ ")"

downloadFile :: Bool -> Bool -> Bool -> Manager -> URL -> Primary -> String
             -> IO (Maybe Bool)
downloadFile dryrun debug done mgr url prime showdestdir = do
  unless debug $ putStrLn url
  if done
    then return (Just False)
    else do
    mtime <- do
      if url /= primaryUrl prime
        then do
        (mirrorSize,mirrorTime) <- retry 3 $ httpFileSizeTime mgr url
        unless (mirrorSize == primarySize prime) $
          putStrLn "Warning!  Mirror filesize differs from primary file"
        unless (mirrorTime == primaryTime prime) $
          putStrLn "Warning!  Mirror timestamp differs from primary file"
        return mirrorTime
        else return $ primaryTime prime
    if dryrun
      then return Nothing
      else do
      tz <- getCurrentTimeZone
      putStrLn $ unwords ["downloading", filename, renderTime tz mtime, "to", showdestdir]
      doCurl
      return (Just True)
  where
    filename = takeFileName url

    doCurl = do
      let subdir = ".dl-fedora-partial"
      whenM (doesFileExist filename) $ do
        partial <- doesFileExist $ subdir </> filename
        if partial
          then warning $ color Yellow "2 partials downloads exist! - better to remove one"
          else renameFile filename (subdir </> filename)
      ok <- curl debug ["--create-dirs", "--output-dir", subdir, "--fail", "--remote-name", url]
      unless ok $ do
        yes <- yesNoDefault True "retry download"
        if yes
          then doCurl
          else error' "download failed"
      whenM (doesFileExist filename) $
        error' $ color Red "file already exists" ++ ": not moving" +-+ subdir </> filename
      renameFile (subdir </> filename) filename

editionMedia :: FedoraEdition -> String
editionMedia Cloud = "images"
editionMedia Container = "images"
editionMedia _ = "iso"

liveRespin :: FedoraEdition -> String
liveRespin Budgie = "Budgie"
liveRespin I3 = "i3"
liveRespin Miracle = "MiracleWM"
liveRespin e = take 4 . upper . showEdition FedoraRespin $ e

bootImage :: Bool -> FilePath -> String -> IO ()
bootImage dryrun img showdir = do
  let fileopts =
        case takeExtension img of
          ".iso" -> ["-boot", "d", "-cdrom"]
          _ -> []
  mQemu <- findExecutable "qemu-kvm"
  case mQemu of
    Just qemu -> do
      let args = ["-m", "4096", "-rtc", "base=localtime", "-cpu", "host"] ++ fileopts
      -- replace with showCommandForUser or cmdN when updated
      putStrLn . unwords $ qemu : map translateInternal (args ++ [showdir </> img])
      unless dryrun $
        cmd_ qemu (args ++ [img])
    Nothing -> error' "Need qemu to run image"

-- from process System.Process.Posix
translateInternal :: String -> String
translateInternal "" = "''"
translateInternal str
   -- goodChar is a pessimistic predicate, such that if an argument is
   -- non-empty and only contains goodChars, then there is no need to
   -- do any quoting or escaping
 | all goodChar str = str
 | otherwise        = '\'' : foldr escape "'" str
  where escape '\'' = showString "'\\''"
        escape c    = showChar c
        goodChar c = isAlphaNum c || c `elem` "-_.,/"

#if !MIN_VERSION_http_directory(0,1,6)
noTrailingSlash :: T.Text -> T.Text
noTrailingSlash = T.dropWhileEnd (== '/')
#endif

#if !MIN_VERSION_http_directory(0,1,9)
infixr 5 +/+
(+/+) :: String -> String -> String
"" +/+ s = s
s +/+ "" = s
s +/+ t | last s == '/' = init s +/+ t
        | head t == '/' = s +/+ tail t
s +/+ t = s ++ "/" ++ t
#endif

-- derived from fedora-repoquery Types
data Arch = Source
          | X86_64
          | AARCH64
          | S390X
          | PPC64LE
  deriving Eq

eitherArch :: String -> Either String Arch
eitherArch s =
  case lower s of
    "source" -> Right Source
    "x86_64" -> Right X86_64
    "aarch64" -> Right AARCH64
    "s390x" -> Right S390X
    "ppc64le" -> Right PPC64LE
    _ -> Left $ "unknown arch: " ++ s

readArch :: String -> Arch
readArch =
  either error' id . eitherArch

showArch :: Arch -> String
showArch Source = "source"
showArch X86_64 = "x86_64"
showArch AARCH64 = "aarch64"
showArch S390X = "s390x"
showArch PPC64LE = "ppc64le"

curl :: Bool -> [String] -> IO Bool
curl debug args = do
  let opts = ["--location", "--continue-at", "-"]
  when debug $
    logMsg $ unwords $ "curl" : opts ++ args
  cmdBool "curl" $ opts ++ args

isCentosStream :: Release -> Bool
isCentosStream (CS _ _) = True
isCentosStream _ = False

csLive :: FedoraEdition -> String
csLive Workstation = "GNOME"
csLive Cinnamon = "CINNAMON"
csLive KDE = "KDE"
csLive MATE = "MATE"
csLive Xfce = "XFCE"
csLive ed = error' $ "unsupported edition:" +-+ showEdition FedoraRespin ed

#if !MIN_VERSION_simple_cmd(0,2,7)
sudoLog :: String -- ^ command
     -> [String] -- ^ arguments
     -> IO ()
sudoLog = sudo_
#endif

reverseSort :: Ord a => [a] -> [a]
reverseSort = sortBy (comparing Down)
