{-# LANGUAGE CPP, TupleSections #-}

module SelectRPMs (
  Select(..),
  selectDefault,
  selectRpmsOptions,
  installArgs,
  checkSelection,
  rpmsToNVRAs,
  Existence(..),
  ExistNVRA,
  Yes(..),
  ExistingStrategy(..),
  existingStrategyOption,
  decideRPMs,
  nvraToRPM,
  groupOnArch,
  PkgMgr(..),
  pkgMgrOpt,
  installRPMs,
  installRPMsAllowErasing
  )
where

import Control.Monad.Extra (forM_, mapMaybeM, unless, when)
import Data.Either (partitionEithers)
import Data.List.Extra (isPrefixOf, isSuffixOf, nubOrd, nubSort, sort,
#if !MIN_VERSION_base(4,20,0)
                        foldl',
#endif
#if MIN_VERSION_extra(1,7,11)
                        groupOnKey,
#endif
                        (\\))
import Data.RPM.NVRA (NVRA(..), readNVRA, showNVRA)
import Safe (headMay)
import SimpleCmd (cmd_, cmdMaybe, error', sudo_, (+-+),
#if MIN_VERSION_simple_cmd(0,2,7)
                  sudoLog
#endif
                 )
import SimpleCmdArgs (Parser, flagWith', flagLongWith', many, strOptionWith,
                      (<|>))
import SimplePrompt (yesNoDefault)
import System.Directory
import System.FilePath ((</>), (<.>))
import System.FilePath.Glob (compile, isLiteral, match)

-- | The Select type specifies the subpackage selection
--
-- Can use name globs: eg "*-devel" or "lib*"
data Select = All -- ^ all packages
            | Ask -- ^ interactive prompting
            | PkgsReq
              [String] -- ^ include matches
              [String] -- ^ except matches
              [String] -- ^ exclude
              [String] -- ^ added
  deriving Eq

-- | Default package selection
selectDefault :: Select
selectDefault = PkgsReq [] [] [] []

-- | An optparse-applicative Parser for Select
selectRpmsOptions :: Parser Select
selectRpmsOptions =
  flagLongWith' All "all" "all subpackages [default if not installed]" <|>
  flagLongWith' Ask "ask" "ask for each subpackage" <|>
  PkgsReq
  <$> many (strOptionWith 'p' "package" "SUBPKG" "select subpackage (glob) matches")
  <*> many (strOptionWith 'e' "except" "SUBPKG" "select subpackages not matching (glob)")
  <*> many (strOptionWith 'x' "exclude" "SUBPKG" "deselect subpackage (glob): overrides -p and -e")
  <*> many (strOptionWith 'i' "include" "SUBPKG" "additional subpackage (glob) to install: overrides -x")

-- | An alternative CLI args option to parse String to Select of rpm packages
installArgs :: String -> Select
installArgs cs =
  case words cs of
    ["-a"] -> All
    ["--all"] -> All
    ["-A"] -> Ask
    ["--ask"] -> Ask
    ws -> installPairs [] [] [] [] ws
  where
    installPairs :: [String] -> [String] -> [String] -> [String]
                 -> [String] -> Select
    installPairs incl except excl add [] = PkgsReq incl except excl add
    installPairs incl except excl add (w:ws)
      | w `elem` ["-p","--package"] =
          case ws of
            [] -> error' "--install opts: --package missing value"
            (w':ws') -> checkPat w' $
                        installPairs (w':incl) except excl add ws'
      | w `elem` ["-e","--except"] =
          case ws of
            [] -> error' "--install opts: --except missing value"
            (w':ws') -> checkPat w' $
                        installPairs incl (w':except) excl add ws'
      | w `elem` ["-x","--exclude"] =
          case ws of
            [] -> error' "--install opts: --exclude missing value"
            (w':ws') -> checkPat w' $
                        installPairs incl except (w':excl) add ws'
      | w `elem` ["-i","--include"] =
          case ws of
            [] -> error' "--install opts: --include missing value"
            (w':ws') -> checkPat w' $
                        installPairs incl except excl (w':add) ws'
      | otherwise = error' "invalid --install opts"

    checkPat w' f =
      if null w'
      then error' "empty pattern!"
      else f

-- FIXME check allowed characters
-- | Check package Select options have no empty strings
--
-- (deprecated export)
checkSelection :: Monad m => Select -> m ()
checkSelection (PkgsReq ps es xs is) =
  forM_ (ps ++ es ++ xs ++ is) $ \s ->
  when (null s) $ error' "empty package pattern not allowed"
checkSelection _ = return ()

-- | Converts a list of RPM files to sorted NVRA's
--
-- (since 0.3.1 no longer excludes debuginfo and debugsource packages)
rpmsToNVRAs :: [String] -> [NVRA]
rpmsToNVRAs = sort . map readNVRA

-- | How to handle already installed subpackages: re-install, skip, or
-- default update
--
-- The default strategy is to select existing subpackages, otherwise all.
--
-- The constructors are only really needed internally but exported for
-- documentation.
data ExistingStrategy = ExistingNoReinstall -- ^ skip reinstall of same NVRs
                      | ExistingSkip        -- ^ skip installed subpkgs
                      | ExistingOnly        -- ^ only update existing subpkgs
                      | ExistingError       -- ^ abort for existing subpkg
  deriving Eq

-- | An optparse-applicative Parser for ExistingStrategy
existingStrategyOption :: Parser ExistingStrategy
existingStrategyOption =
  flagWith' ExistingNoReinstall 'N' "no-reinstall" "Do not reinstall existing NVRs" <|>
  flagWith' ExistingSkip 'S' "skip-existing" "Ignore already installed subpackages (implies --no-reinstall)" <|>
  flagWith' ExistingOnly 'O' "only-existing" "Only update existing installed subpackages" <|>
  flagWith' ExistingError 'E' "error-existing" "Abort for existing installed subpackages"

-- | Sets prompt default behaviour for yes/no questions
data Yes = No | Yes
  deriving Eq

-- | Current state of a package NVR
data Existence = ExistingNVR -- ^ NVR is already installed
               | ChangedNVR -- ^ NVR is different to installed package
               | NotInstalled -- ^ package is not currently installed
  deriving (Eq, Ord, Show)

-- | Combines Existence state with an NVRA
type ExistNVRA = (Existence, NVRA)

-- FIXME determine and add missing internal deps
-- | Decide list of NVRs based on a Select selection (using a package prefix)
decideRPMs :: Yes -- ^ prompt default choice
           -> Bool -- ^ enable list mode which just displays the package list
           -> Maybe ExistingStrategy -- ^ optional existing install strategy
           -> Select -- ^ specifies package Select choices
           -> String -- ^ package set prefix: allows Select'ing without prefix
           -> [NVRA] -- ^ list of rpm packages to select from
           -> IO [ExistNVRA] -- ^ returns list of selected rpm packages
decideRPMs yes listmode mstrategy select prefix nvras = do
  checkSelection select
  classified <- mapMaybeM installExists (filter isBinaryRpm nvras)
  if listmode
    then do
    mapM_ printInstalled $
      case select of
        PkgsReq subpkgs exceptpkgs exclpkgs addpkgs ->
          selectRPMs prefix (subpkgs,exceptpkgs,exclpkgs,addpkgs) classified
        _ -> classified
    return []
    else
    case select of
      All -> promptPkgs mstrategy yes classified
      Ask -> mapMaybeM (rpmPrompt yes) classified
      PkgsReq subpkgs exceptpkgs exclpkgs addpkgs ->
        promptPkgs mstrategy yes $
        selectRPMs prefix (subpkgs,exceptpkgs,exclpkgs,addpkgs) classified
  where
    installExists :: NVRA -> IO (Maybe ExistNVRA)
    installExists nvra = do
      -- FIXME this will fail for noarch changes
      -- FIXME check kernel
      minstalled <- cmdMaybe "rpm" ["-q", rpmName nvra <.> rpmArch nvra]
      let existence =
            case minstalled of
              Nothing -> NotInstalled
              Just installed ->
                if showNVRA nvra `elem` lines installed
                then ExistingNVR
                else ChangedNVR
      return $
        case mstrategy of
          Just ExistingSkip | existence /= NotInstalled -> Nothing
          Just ExistingNoReinstall | existence == ExistingNVR -> Nothing
          Just ExistingOnly | existence == NotInstalled -> Nothing
          Just ExistingError | existence /= NotInstalled -> error' $ "aborting:" +-+ rpmName nvra +-+ "already installed"
          _ -> Just (existence, nvra)

-- FIXME move to submodule?
selectRPMs :: String
           -- (subpkgs,except,exclpkgs,addpkgs)
           -> ([String],[String],[String],[String])
           -> [ExistNVRA] -> [ExistNVRA]
selectRPMs prefix (subpkgs,exceptpkgs,exclpkgs,addpkgs) rpms =
  let excluded = matchingRPMs prefix exclpkgs rpms
      included = matchingRPMs prefix addpkgs rpms
      matching =
        if null subpkgs && null exceptpkgs
        then defaultRPMs prefix rpms
        else matchingRPMs prefix subpkgs rpms
      nonmatching = nonMatchingRPMs prefix exceptpkgs rpms
  in nubSort $ ((matching ++ nonmatching) \\ excluded) ++ included

isBinaryRpm :: NVRA -> Bool
isBinaryRpm = (/= "src") . rpmArch

renderInstalled :: ExistNVRA -> String
renderInstalled (exist, nvra) =
  case exist of
    ExistingNVR -> '='
    ChangedNVR -> '^'
    NotInstalled -> '+'
  : showNVRA nvra

printInstalled :: ExistNVRA -> IO ()
printInstalled = putStrLn . renderInstalled

promptPkgs :: Maybe ExistingStrategy -> Yes -> [ExistNVRA] -> IO [ExistNVRA]
promptPkgs (Just ExistingOnly) _ [] = do
  putStrLn "skipped"
  return []
promptPkgs _ _ [] = error' "no rpms found"
promptPkgs _ yes classified = do
  mapM_ printInstalled classified
  ok <- prompt yes "install above"
  return $ if ok then classified else []

prompt :: Yes -> String -> IO Bool
prompt yes str = do
  if yes == Yes
    then return True
    else yesNoDefault True str

rpmPrompt :: Yes -> ExistNVRA -> IO (Maybe ExistNVRA)
rpmPrompt yes epn = do
  ok <- prompt yes $ renderInstalled epn
  return $
    if ok
    then Just epn
    else Nothing

defaultRPMs :: String -> [ExistNVRA] -> [ExistNVRA]
defaultRPMs prefix rpms =
  let installed = filter ((/= NotInstalled) . fst) rpms
  in if null installed
     then filter (wantedSubpackage . rpmName . snd) rpms
     else installed
  where
    wantedSubpackage :: String -> Bool
    wantedSubpackage p =
      notDebugPkg p && defaultSubpackage p

    notDebugPkg :: String -> Bool
    notDebugPkg p =
      not ("-debuginfo" `isSuffixOf` p || "-debugsource" `isSuffixOf` p)

    defaultSubpackage :: String -> Bool
    defaultSubpackage p =
      not ("ghc" `isPrefixOf` prefix)
      ||
      not ("-doc" `isSuffixOf` p || "-prof" `isSuffixOf` p || "compiler-default" `isSuffixOf` p)

-- FIXME add --strict (must match) switch
matchingRPMs :: String -> [String] -> [ExistNVRA] -> [ExistNVRA]
matchingRPMs prefix subpkgs rpms =
  nubSort . mconcat $
  flip map (nubOrd subpkgs) $ \pkgpat ->
  case getMatches pkgpat of
    [] -> if headMay pkgpat /= Just '*'
          then getMatches (prefix ++ '-' : pkgpat)
          else []
    result -> result
  where
    getMatches :: String -> [ExistNVRA]
    getMatches pkgpat =
      filter (match (compile pkgpat) . rpmName . snd) rpms

nonMatchingRPMs :: String -> [String] -> [ExistNVRA] -> [ExistNVRA]
nonMatchingRPMs _ [] _ = []
nonMatchingRPMs prefix subpkgs rpms =
  -- FIXME somehow determine unused excludes
  nubSort $ foldl' (exclude (nubOrd subpkgs)) [] rpms
  where
    rpmnames = map (rpmName . snd) rpms

    exclude :: [String] -> [ExistNVRA] -> ExistNVRA
            -> [ExistNVRA]
    exclude [] acc rpm = acc ++ [rpm]
    exclude (pat:pats) acc rpm =
        if checkMatch (rpmName (snd rpm))
        then acc
        else exclude pats acc rpm
      where
        checkMatch :: String -> Bool
        checkMatch rpmname =
          let comppat = compile pat
          in if isLiteral comppat
             then pat == rpmname ||
                  pat `notElem` rpmnames &&
                  (prefix ++ '-' : pat) == rpmname
             else match comppat rpmname

-- | Whether a package needs to be reinstalled or installed
data InstallType = ReInstall
                 | Install

-- | Package manager
data PkgMgr = DNF3 | DNF5 | RPM | OSTREE
  deriving Eq

-- | An optparse-applicative Parser for PkgMgr
--
-- (since 0.3.1)
pkgMgrOpt :: Parser PkgMgr
pkgMgrOpt =
  flagLongWith' RPM "rpm" "Use rpm instead of dnf" <|>
  flagLongWith' OSTREE "rpm-ostree" "Use rpm-ostree instead of dnf" <|>
  flagLongWith' DNF5 "dnf5" "Use dnf5 to install" <|>
  flagLongWith' DNF3 "dnf3" "Use dnf-3 to install [default dnf unless ostree]"

-- | Do installation of selected rpm packages
installRPMs :: Bool -- ^ dry-run
            -> Bool -- ^ debug output
            -> Maybe PkgMgr -- ^ optional specify package manager
            -> Yes -- ^ prompt default choice
            -> [(FilePath,[ExistNVRA])] -- ^ list of rpms to install with path
            -> IO ()
installRPMs dryrun debug mmgr =
  installRPMsAllowErasing dryrun debug mmgr False

-- FIXME support options per build: install ibus imsettings -i plasma
-- (or don't error if multiple packages)
-- | Do installation of packages (with allowerasing switch)
--
-- (since 0.3.1)
installRPMsAllowErasing :: Bool -- ^ dry-run
                        -> Bool -- ^ debug output
                        -> Maybe PkgMgr -- ^ optional specify package manager
                        -> Bool -- ^ use dnf --allowerasing
                        -> Yes -- ^ prompt default choice
                        -> [(FilePath,[ExistNVRA])] -- ^ list of rpms to install with path
                        -> IO ()
installRPMsAllowErasing _ _ _ _ _ [] = return ()
installRPMsAllowErasing dryrun debug mmgr allowerasing yes classifieds =
  case installTypes (concatMap zipDir classifieds) of
    ([],is) -> doInstall Install is
    (ris,is) -> do
      doInstall ReInstall (ris ++ is) -- include any new deps
      doInstall Install is            -- install any non-deps
  where
    zipDir :: (FilePath,[ExistNVRA]) -> [(FilePath,ExistNVRA)]
    zipDir (dir, rpms) = map (dir,) rpms

    installTypes :: [(FilePath,ExistNVRA)]
                 -> ([(FilePath,NVRA)],[(FilePath,NVRA)])
    installTypes = partitionEithers . map partExist
      where
        partExist :: (FilePath,ExistNVRA)
                  -> Either (FilePath,NVRA) (FilePath,NVRA)
        partExist (d,(e,n)) = (if e == ExistingNVR then Left else Right) (d,n)

    doInstall :: InstallType -> [(FilePath,NVRA)] -> IO ()
    doInstall inst dirpkgs =
      unless (null dirpkgs) $ do
      mgr <-
        case mmgr of
          Just m -> return m
          Nothing -> do
            ostree <- doesDirectoryExist "/sysroot/ostree"
            if ostree
              then return OSTREE
              else do
              mdnf5 <- findExecutable "dnf5"
              return $ maybe DNF3 (const DNF5) mdnf5
      let pkgmgr =
            case mgr of
              DNF3 -> "dnf-3"
              DNF5 -> "dnf5"
              RPM -> "rpm"
              OSTREE -> "rpm-ostree"
          com =
            case inst of
              ReInstall -> reinstallCommand mgr
              Install -> installCommand mgr
        in
        if dryrun
        then mapM_ putStrLn $ ("would" +-+ unwords (pkgmgr : com) ++ ":") : map showRpmFile dirpkgs
        else do
          when debug $ mapM_ (putStrLn . showRpmFile) dirpkgs
          (case mgr of
            OSTREE -> cmd_
            _ -> if debug then sudoLog else sudo_) pkgmgr $
            com ++ map showRpmFile dirpkgs ++ ["--allowerasing" | allowerasing] ++ ["--assumeyes" | yes == Yes && mgr `elem` [DNF3,DNF5]]

    reinstallCommand :: PkgMgr -> [String]
    reinstallCommand mgr =
      case mgr of
        DNF3 -> ["reinstall"]
        DNF5 -> ["reinstall"]
        RPM -> ["-Uvh","--replacepkgs"]
        OSTREE -> ["install"]

    installCommand :: PkgMgr -> [String]
    installCommand mgr =
      case mgr of
        DNF3 -> ["localinstall"]
        DNF5 -> ["install"]
        RPM -> ["-Uvh"]
        OSTREE -> ["install"]

-- FIXME replace with export from rpm-nvr (once released)
-- | Render a NVRA as rpm file
nvraToRPM :: NVRA -> FilePath
nvraToRPM nvra = showNVRA nvra <.> "rpm"

-- | Render path and NVRA are rpm filepath
showRpmFile :: (FilePath,NVRA) -> FilePath
showRpmFile (dir,nvra) = dir </> nvraToRPM nvra

-- | Group rpms by arch (subdirs)
groupOnArch :: FilePath -- ^ prefix directory (eg "RPMS")
            -> [ExistNVRA]
            -> [(FilePath,[ExistNVRA])]
groupOnArch dir = groupOnKey (\(_,p) -> dir </> rpmArch p)

#if !MIN_VERSION_extra(1,7,11)
groupOnKey :: Eq k => (a -> k) -> [a] -> [(k, [a])]
groupOnKey _ []     = []
groupOnKey f (x:xs) = (fx, x:yes) : groupOnKey f no
    where
        fx = f x
        (yes, no) = span (\y -> fx == f y) xs
#endif

#if !MIN_VERSION_simple_cmd(0,2,7)
sudoLog :: String -- ^ command
     -> [String] -- ^ arguments
     -> IO ()
sudoLog = sudo_
#endif
