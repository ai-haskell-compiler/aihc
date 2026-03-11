{-# LANGUAGE BangPatterns, OverloadedStrings, CPP #-}

import Control.Monad.Extra (unless, unlessM, when, whenJustM, (||^))
import Data.Either
import Data.Ini.Config
import Data.List.Extra -- not explicit to avoid awkward CPP
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Tuple (swap)
import Data.Version.Extra
import SimpleCmd ((+-+), cmdBool, cmd, cmd_, error', needProgram, warning,
#if MIN_VERSION_simple_cmd(0,2,4)
                  fileWithExtension
#endif
                 )
import SimpleCmdArgs
import System.Directory (copyFile, doesFileExist, getCurrentDirectory,
#if !MIN_VERSION_simple_cmd(0,2,4)
                         listDirectory,
#endif
                         setCurrentDirectory, withCurrentDirectory)
import System.Exit
#if !MIN_VERSION_simple_cmd(0,2,4)
import System.FilePath -- not explicit to avoid awkward CPP
#endif
import System.IO
import System.Process
import qualified Data.Text.IO as T

import MajorVer
import Paths_stack_all (version)
import Snapshots
import StackYaml

defaultOldestLTS :: MajorVer
defaultOldestLTS = LTS 20

data VersionLimit = DefaultLimit | Oldest MajorVer | AllVersions

data Command = CreateConfig
             | SetDefaultResolver MajorVer
             | UpdateDefaultResolver
             | MakeStackLTS MajorVer
             | MakeAllLTS
             | RunCmd [String]
  deriving Eq

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  simpleCmdArgs' (Just version) "Build project over Stackage major versions"
    "https://github.com/juhp/stack-all#readme" $
    run
    <$> switchWith 'k' "keep-going" "Keep going even if an LTS fails"
    <*> switchWith 'D' "debug" "Verbose stack build output on error"
    <*> switchLongWith "refresh-cache" "Force refresh of stackage snapshots.json cache"
    <*> optional (strOptionLongWith "stack" "STACKPROG" "stack program to use [default: stack]")
    <*> optional (readMajor <$> strOptionWith 'n' "newest" "MAJOR" "Newest LTS release to build from")
    <*> (Oldest . readMajor <$> strOptionWith 'o' "oldest" "MAJOR" "Oldest compatible LTS release" <|>
         flagWith DefaultLimit AllVersions 'a' "all-lts" "Try to build back to LTS 1 even")
    <*>
    (flagWith' CreateConfig 'c' "create-config" "Create a project .stack-all file" <|>
     (SetDefaultResolver . readMajor <$> strOptionWith 'r' "default-resolver" "MAJOR" ("Set" +-+ stackYaml +-+ "resolver")) <|>
     flagWith' UpdateDefaultResolver 'u' "update-resolver" ("Update" +-+ stackYaml +-+ "resolver") <|>
     (MakeStackLTS . readMajor <$> strOptionWith 's' "make-lts" "MAJOR" "Create a stack-ltsXX.yaml file") <|>
     flagWith' MakeAllLTS 'S' "make-all-lts" "Create all stack-ltsXX.yaml files" <|>
     RunCmd <$> many (strArg "MAJORVER... COMMAND..."))

run :: Bool ->Bool -> Bool -> Maybe FilePath -> Maybe MajorVer -> VersionLimit
    -> Command -> IO ()
run keepgoing debug refresh mstack mnewest verlimit com = do
  whenJustM findStackProjectDir setCurrentDirectory
  case com of
    CreateConfig ->
      case verlimit of
        Oldest oldest -> createStackAll (Just oldest) mnewest
        _ -> createStackAll Nothing mnewest
    SetDefaultResolver ver -> stackDefaultResolver $ Just ver
    UpdateDefaultResolver -> stackDefaultResolver Nothing
    MakeStackLTS ver -> makeStackLTS False refresh ver
    MakeAllLTS -> do
      vers <- determineVersions mnewest verlimit []
      mapM_ (makeStackLTS (length vers > 1) refresh) vers
    RunCmd verscmd -> do
      (versions, cargs) <- getVersionsAndCmd verscmd
      configs <- readStackConfigs
      needProgram $ fromMaybe "stack" mstack
      let finalvers = maybe id (filter . (>=)) mnewest versions
      mapM_ (runStack configs keepgoing debug refresh mstack $ if null cargs then ["build"] else cargs) finalvers
  where
    findStackProjectDir :: IO (Maybe FilePath)
    findStackProjectDir = do
      haveStackYaml <- doesFileExist stackYaml
      cwdir <- getCurrentDirectory
      if haveStackYaml
        then return $ Just cwdir
        else do
        haveCabalPackageFile <-
          doesFileExistWithExtension "." ".cabal" ||^
          doesFileExist "package.yaml" ||^
          doesFileExist "cabal.project"
        if haveCabalPackageFile
          then do
          putStrLn $ "creating" +-+ stackYaml +-+ "in" +-+ cwdir
          -- FIXME take suggested extra-deps into stack.yaml
          -- FIXME stack init content too verbose
          let stack = fromMaybe "stack" mstack
          needProgram stack
          unlessM (cmdBool stack ["init"]) $ do
            snap <- latestLtsSnapshot refresh
            writeFile stackYaml $ "resolver: " ++ snap ++ "\n"
          return $ Just cwdir
          else
          if cwdir /= "/"
          then withCurrentDirectory ".." findStackProjectDir
          else do
          putStrLn "no package/project found"
          return Nothing

    getVersionsAndCmd :: [String] -> IO ([MajorVer],[String])
    getVersionsAndCmd verscmd = do
      let partitionMajors = swap . partitionEithers . map eitherReadMajorAlias
          (verlist,cmds) = partitionMajors verscmd
      versions <- determineVersions mnewest verlimit verlist
      return (versions,cmds)

inRange :: MajorVer -> MajorVer -> MajorVer -> Bool
inRange newest oldest v = v >= oldest && v <= newest

determineVersions :: Maybe MajorVer -> VersionLimit -> [MajorVerAlias]
                  -> IO [MajorVer]
determineVersions mnewest verlimit verlist = do
        if null verlist then do
          allMajors <- getMajorVers
          case verlimit of
            DefaultLimit -> do
              (newestLTS, oldestLTS) <- readNewestOldestLTS mnewest
              return $
                case mnewest of
                  Just newest ->
                    if newest < oldestLTS
                    then filter (inRange newest (LTS 1)) allMajors
                    else filter (inRange newest oldestLTS) allMajors
                  Nothing -> filter (inRange newestLTS oldestLTS) allMajors
            AllVersions -> return allMajors
            Oldest ver ->
              return $
              filter (inRange (fromMaybe Nightly mnewest) ver) allMajors
        else nub <$> mapM resolveMajor verlist

readNewestOldestLTS :: Maybe MajorVer -> IO (MajorVer,MajorVer)
readNewestOldestLTS mnewest = do
  haveConfig <- doesFileExist stackAllFile
  if haveConfig then
    readIniConfig stackAllFile $
    section "versions" $ do
    mnewest' <-
      case mnewest of
        Nothing -> fmap readMajor <$> fieldMbOf "newest" string
        _ -> return mnewest
    moldest' <- fmap readMajor <$> fieldMbOf "oldest" string
    return (fromMaybe Nightly mnewest', fromMaybe defaultOldestLTS moldest')
    else return (fromMaybe Nightly mnewest, defaultOldestLTS)
  where
    readIniConfig :: FilePath -> IniParser a -> IO a
    readIniConfig inifile iniparser = do
      ini <- T.readFile inifile
      return $ either error id $ parseIniFile ini iniparser

stackAllFile :: FilePath
stackAllFile = ".stack-all"

createStackAll :: Maybe MajorVer -> Maybe MajorVer -> IO ()
createStackAll Nothing Nothing =
  error' "creating .stack-all requires --oldest LTS and/or --newest LTS"
createStackAll moldest mnewest = do
  exists <- doesFileExist stackAllFile
  if exists then error' $ stackAllFile ++ " already exists"
    else do
    allMajors <- getMajorVers
    writeFile stackAllFile $
      "[versions]\n" ++
      case mnewest of
        Nothing -> ""
        Just newest ->
          "newest = " ++ showMajor newest ++ "\n"
      ++
      case moldest of
        Nothing -> ""
        Just oldest ->
          let older =
                let molder = listToMaybe $ dropWhile (>= oldest) allMajors
                in maybe "" (\s -> showMajor s ++ " too old") molder
          in "# " ++ older ++ "\noldest = " ++ showMajor oldest ++ "\n"

updateResolver :: String -> FilePath -> IO ()
updateResolver snapshot stackfile =
  cmd_ "sed" ["-i", "-e", "s/\\(resolver:\\|snapshot:\\) .*/\\1 " ++ snapshot ++ "/", stackfile]

stackDefaultResolver :: Maybe MajorVer -> IO ()
stackDefaultResolver mver = do
  unlessM (doesFileExist stackYaml) $
    error' $ "no" +-+ stackYaml +-+ "present"
  case mver of
    Nothing -> do
      mdef <- readDefaultStackYaml
      case mdef of
        Nothing -> error' $ "could not determine major version of" +-+ stackYaml
        Just ver -> stackDefaultResolver $ Just ver
    Just ver ->
      whenJustM (latestMajorSnapshot False ver) $ \latest ->
      updateResolver latest stackYaml

makeStackLTS :: Bool -> Bool -> MajorVer -> IO ()
makeStackLTS multiple refresh ver = do
  configs <- readStackConfigs
  case filter (isVersion ver) configs of
    [] -> do
      let newfile = configFile $ Config ver
          mcurrentconfig = find ((ver <) . configVersion) configs
      when multiple $ putStrLn $ "creating" +-+ newfile
      case mcurrentconfig of
        Nothing -> copyFile stackYaml newfile
        Just conf -> do
          let origfile = configFile conf
          copyFile origfile newfile
      whenJustM (latestMajorSnapshot refresh ver) $ \latest ->
        updateResolver latest newfile
    [c] ->
      let msg =
            configFile c +-+ "already" +-+
            case c of
              Config _ -> "exists"
              ConfigDefault v -> "has" +-+ showMajor v
      in if multiple
         then putStrLn msg
         else error' $ msg ++ "!"
    cs ->
      (if multiple then putStrLn else error') $ "overlapping major versions:" +-+
      unwords (map configFile cs)

runStack :: [ConfigVersion] -> Bool -> Bool -> Bool -> Maybe FilePath -> [String]
         -> MajorVer -> IO ()
runStack configs keepgoing debug refresh mstack command ver = do
  stack <-
    case mstack of
      Just stk -> return stk
      Nothing ->
        if ver <= LTS 11
        then do
          stackver <- readVersion <$> cmd "stack" ["--numeric-version"]
          if stackver >= makeVersion [3]
            then do
            warning $ "Warning: found stack version" +-+ showVersion stackver ++ ", which does not support lts < 12"
            warning "Trying stack-2.15.7 (otherwise use '--stack stack-2.*')"
            return "stack-2.15.7"
            else return "stack"
        else return "stack"
  let !mconfig =
        case filter (isVersion ver) configs of
          [] ->
            case sort (filter (Config ver <) $ delete (Config Nightly) configs) of
              [] -> Nothing
              (c:_) -> nonDefaultConfig c
          [c] -> nonDefaultConfig c
          cs -> error' $ "overlapping major versions:" +-+
                unwords (map configFile cs)
  latest <- latestMajorSnapshot refresh ver
  case latest of
    Nothing -> error' $ "no snapshot not found for " ++ showMajor ver
    Just minor -> do
      let opts = ["-v" | debug] ++ ["--resolver", minor] ++
                 maybe [] (\f -> ["--stack-yaml", configFile f]) mconfig
      putStrLn $ "# " ++ minor
      if debug
        then debugBuild stack $ opts ++ command
        else do
        ok <- cmdBool stack $ opts ++ command
        unless (ok || keepgoing) $ do
          putStr "\nsnapshot-pkg-db: "
          cmd_ stack $ "--silent" : opts ++ ["path", "--snapshot-pkg-db"]
          error' $ "failed for " ++ showMajor ver
      putStrLn ""
  where
    debugBuild :: FilePath -> [String] -> IO ()
    debugBuild stack args = do
      putStr $ stack +-+ unwords args
      (ret,out,err) <- readProcessWithExitCode stack args ""
      putStrLn "\n"
      unless (null out) $ putStrLn out
      unless (ret == ExitSuccess) $ do
        -- stack verbose includes info line with all stackages (> 500kbytes)
        mapM_ putStrLn $ filter ((<10000) . length) . lines $ err
        error' $ showMajor ver ++ " build failed"


#if !MIN_VERSION_simple_cmd(0,2,4)
#if !MIN_VERSION_filepath(1,4,2)
isExtensionOf :: String -> FilePath -> Bool
isExtensionOf ext@('.':_) = isSuffixOf ext . takeExtensions
isExtensionOf ext         = isSuffixOf ('.':ext) . takeExtensions
#endif

filesWithExtension :: FilePath -- directory
                   -> String   -- file extension
                   -> IO [FilePath]
filesWithExtension dir ext =
  filter (ext `isExtensionOf`) <$> listDirectory dir

-- looks in dir for a unique file with given extension
fileWithExtension :: FilePath -- directory
                  -> String   -- file extension
                  -> IO (Maybe FilePath)
fileWithExtension dir ext = do
  files <- filesWithExtension dir ext
  case files of
       [file] -> return $ Just $ dir </> file
       [] -> return Nothing
       _ -> putStrLn ("More than one " ++ ext ++ " file found!") >> return Nothing
#endif

-- looks in current dir for a unique file with given extension
doesFileExistWithExtension :: FilePath -> String -> IO Bool
doesFileExistWithExtension dir ext = do
  mf <- fileWithExtension dir ext
  case mf of
    Nothing -> return False
    Just f -> doesFileExist f
