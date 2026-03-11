-- |
-- Module      : Debian.Package.Build.Command
-- Copyright   : 2014-2015 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- This module provides trace-able action instances like commands.
module Debian.Package.Build.Command
       ( chdir, pwd

       , createDirectoryIfMissing, renameDirectory, renameFile

       , confirmPath

       , unpackInDir, unpack, packInDir', packInDir

       , cabalDebian', cabalDebian, packageVersion
       , dpkgParseChangeLog, dpkgParseControl

       , debuild, debi', debi, aptGetBuildDepends

       , BuildMode (..)

       , modeListFromControl, buildPackage, build, rebuild

       , removeGhcLibrary

       , withCurrentDir'

       , readProcess', rawSystem', system'
       ) where

import Data.Maybe (fromMaybe)
import Control.Applicative ((<$>))
import Control.Monad (when, unless)
import Control.Monad.Trans.Class (lift)
import System.FilePath ((</>), (<.>), takeDirectory)
import qualified System.Directory as D
import qualified System.Process as Process
import System.Exit (ExitCode (..))
import Data.Version (versionBranch, showVersion)

import Debian.Package.Data
  (Hackage, ghcLibraryBinPackages, ghcLibraryPackages, ghcLibraryDocPackage,
   Source, parseChangeLog, DebianVersion, readDebianVersion, origVersion',
   Control (..), parseControl)
import Debian.Package.Build.Monad (Trace, traceCommand, traceOut, putLog, bracketTrace_)


handleExit :: String -> ExitCode -> IO ()
handleExit cmd = d  where
  d (ExitFailure rv) = fail $ unwords ["Failed with", show rv ++ ":", cmd]
  d  ExitSuccess     = return ()

-- | Run command without shell and get standard output string.
readProcess' :: String -> [String] -> String -> Trace String
readProcess' cmd args in' = do
  traceCommand . unwords $ cmd : args
  lift $ Process.readProcess cmd args in'

-- | Run command without shell
rawSystem' :: String -> [String] -> Trace ()
rawSystem' cmd args = do
  traceCommand . unwords $ cmd : args
  lift (Process.rawSystem cmd args >>= handleExit cmd)

-- | Run command with shell
system' :: String -> Trace ()
system' cmd = do
  traceCommand cmd
  lift $ Process.system cmd >>= handleExit cmd

-- | Change directory action
chdir :: String -> Trace ()
chdir dir =  do
  traceCommand $ "<setCurrentDirectory> " ++ dir
  lift $ D.setCurrentDirectory dir

-- | Action to get current working directory
pwd :: IO String
pwd =  D.getCurrentDirectory

-- | Create directory if missing
createDirectoryIfMissing :: String -> Trace ()
createDirectoryIfMissing dir = do
  traceCommand $ "<createDirectoryIfMissing True> " ++ dir
  lift $ D.createDirectoryIfMissing True dir

renameMsg :: String -> String -> String -> String
renameMsg tag src dst = unwords ["<" ++ tag ++ "> ", src, "-->", dst]

-- | Rename directory action. e.g. /renameDirectory from to/
renameDirectory :: String -> String -> Trace ()
renameDirectory src dst = do
  traceCommand $ renameMsg "renameDirectory" src dst
  lift $ D.renameDirectory src dst

-- | Rename file action. e.g. /renameFile from to/
renameFile :: String -> String -> Trace ()
renameFile src dst = do
  traceCommand $ renameMsg "renameFile" src dst
  lift $ D.renameFile src dst

-- | Confirm filepath using /ls/ command
confirmPath :: String -> Trace ()
confirmPath path =
  readProcess' "ls" ["-ld", path] "" >>= traceOut


-- | Unpack .tar.gz under directory.
unpackInDir :: FilePath -> FilePath -> Trace ()
apath `unpackInDir` dir = do
  putLog $ unwords ["Unpacking", apath, "in", dir, "."]
  rawSystem' "tar" ["-C", dir, "-zxf", apath]

-- | Unpack .tar.gz under archive place.
unpack :: FilePath -> Trace ()
unpack apath = apath `unpackInDir` takeDirectory apath

-- | Pack directory into .tar.gz under working directory
packInDir' :: FilePath -> FilePath -> FilePath -> Trace ()
packInDir' pdir apath wdir = do
  putLog $ unwords ["Packing", pdir, "in", wdir, "into", apath, "."]
  rawSystem' "tar" ["-C", wdir, "-zcf", apath, pdir]

-- | Pack directory into same location .tar.gz under working directory
packInDir :: FilePath -> FilePath -> Trace ()
pdir `packInDir` wdir =
  packInDir' pdir (pdir <.> "tar" <.> "gz") wdir


-- | Run action under specified directory
withCurrentDir' :: FilePath -> Trace a -> Trace a
withCurrentDir' dir act = do
  saveDir <- lift pwd
  bracketTrace_
    (chdir dir)
    (chdir saveDir)
    act

-- | Just call /cabal-debian/ command
cabalDebian' :: Maybe String -> [String] -> Trace ()
cabalDebian' mayRev otherArgs = do
  ver <-  origVersion' <$> packageVersion "cabal-debian"
  let revision = fromMaybe "1~autogen1" mayRev
      oldArgs = ["--quilt", "--revision=" ++ revision]
      verBranch = take 2 $ versionBranch ver ++ [0,0]

  args <- case verBranch of
    (x:y:_)  | x <= 1             ->  fail
                                      $ "Version of cabal-debian is TOO OLD: "
                                      ++ "'" ++ showVersion ver ++ "'"
                                      ++ " - Under version 1 generates wrong dependencies."
             | 2 <= x && x <= 3   ->  return oldArgs
             | x == 4 && y <  19  ->  return oldArgs
             | x >= 4             ->  return ["--revision=" ++ '-' : revision]
             | otherwise          ->  fail
                                      $ "unknown version: "
                                      ++ "'" ++ showVersion ver ++ "'"
    _                             ->  fail
                                      $ "unexpected version format: "
                                      ++ "'" ++ showVersion ver ++ "'"

  rawSystem' "cabal-debian" $ args ++ otherArgs

-- | Call /cabal-debian/ command under specified directory
cabalDebian :: FilePath -> Maybe String -> [String] -> Trace ()
cabalDebian dir mayRev = withCurrentDir' dir . cabalDebian' mayRev

-- | Query debian package version
packageVersion :: String -> Trace DebianVersion
packageVersion pkg = do
  vstr <- readProcess' "dpkg-query" ["--show", "--showformat=${Version}", pkg] ""
  maybe (fail $ "readDebianVersion: failed: " ++ vstr) return
    $ readDebianVersion vstr

-- | Read debian changelog file and try to parse into 'Source'
dpkgParseChangeLog :: FilePath -> Trace Source
dpkgParseChangeLog cpath =  do
  str <- readProcess' "dpkg-parsechangelog" ["-l" ++ cpath] ""
  maybe (fail $ "parseChangeLog: failed: " ++ str) return
    $ parseChangeLog str

-- | Read debian control file
dpkgParseControl :: FilePath -> Trace Control
dpkgParseControl cpath =  do
  putLog $ unwords ["Reading", cpath, "."]
  str <- lift $ readFile cpath
  maybe (fail $ "parseControl: failed: " ++ str) return
    $ parseControl str


debuild' :: [String] -> Trace ()
debuild' =  rawSystem' "debuild"

-- | Call /debuild/ under specified directory, with command line options
debuild :: FilePath -> [String] -> Trace ()
debuild dir = withCurrentDir' dir . debuild'

-- | Just run debi with root user
debi' :: [String] -> Trace ()
debi' =  rawSystem' "sudo" . ("debi" :)

-- | Install packages under specified source package directory
debi :: FilePath -> [String] -> Trace ()
debi dir = withCurrentDir' dir . debi'

-- | Install build-depends
aptGetBuildDepends :: FilePath -> Trace ()
aptGetBuildDepends dir =
  withCurrentDir' dir $ rawSystem' "sudo" ["apt-get-build-depends"]

-- | Build mode, all or binary only
data BuildMode = All | Bin | Src | Dep | Indep
               deriving (Eq, Show, Read, Bounded, Enum)

-- | Infer all build mode list from debian control file data
modeListFromControl :: Control -> [BuildMode]
modeListFromControl c =
  Src
  :  [ Dep   | not . null $ controlArch c ]
  ++ [ Indep | not . null $ controlAll  c ]

hasBinaryBuildMode :: BuildMode -> Bool
hasBinaryBuildMode =  not . (== Src)

-- | Build package using /debuild/ under specified directory
buildPackage :: FilePath -> BuildMode -> [String] -> Trace ()
buildPackage dir mode opts = do
  let modeOpt All    =  []
      modeOpt Bin    =  ["-b"]
      modeOpt Src    =  ["-S"]
      modeOpt Dep    =  ["-B"]
      modeOpt Indep  =  ["-A"]
  debuild dir $ ["-uc", "-us"] ++ modeOpt mode ++ opts

-- | Build package with specified mode list.
--   Calculated mode list from control is used when not specified build modes.
build :: FilePath -> [BuildMode] -> Bool -> [String] -> Trace ()
build dir modes' installDep opts = do
  modes <-
    if null modes'
    then modeListFromControl <$> dpkgParseControl (dir </> "debian" </> "control")
    else return modes'
  when (installDep && any hasBinaryBuildMode modes) $ aptGetBuildDepends dir
  sequence_ [buildPackage dir m opts | m <- modes]

-- | Clean and build package using /debuild/ under specified directory
rebuild :: FilePath -> [BuildMode] -> [String] -> Trace ()
rebuild dir modes opts = do
  debuild dir ["clean"]
  build dir modes False opts

-- | Remove ghc library packages under specified source package directory
removeGhcLibrary :: BuildMode -> Hackage -> Trace ()
removeGhcLibrary mode hkg = do
  let pkgs All   =  ghcLibraryPackages
      pkgs Bin   =  ghcLibraryPackages
      pkgs Src   =  const []
      pkgs Dep   =  ghcLibraryBinPackages
      pkgs Indep =  (:[]) . ghcLibraryDocPackage
      pkgs' = pkgs mode hkg
  unless (null pkgs') . system'
    $ unwords ["echo '' |", "sudo apt-get remove", unwords pkgs', "|| true"]
