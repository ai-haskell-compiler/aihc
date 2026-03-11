{-# LANGUAGE OverloadedStrings #-}

module MemInfo.Files.Root (
  -- * functions
  useTmp,
  writeRootedFile,
  initRoot,
  clearDirectory,
) where

import Data.Text (Text)
import qualified Data.Text.IO as Text
import Fmt ((+|), (|+))
import System.Directory (
  createDirectoryIfMissing,
  listDirectory,
  removePathForcibly,
 )
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import System.MemInfo.SysInfo (
  KernelVersion,
 )


useTmp :: (FilePath -> IO a) -> IO a
useTmp = withSystemTempDirectory "mem-info"


writeRootedFile :: FilePath -> FilePath -> Text -> IO ()
writeRootedFile root path txt = do
  let target = root </> path
  createDirectoryIfMissing True $ takeDirectory target
  Text.writeFile target txt


initRoot :: FilePath -> KernelVersion -> IO ()
initRoot root version = do
  clearDirectory root
  writeRootedFile root "sys/kernel/osrelease" $ fmtKernelVersion version


clearDirectory :: FilePath -> IO ()
clearDirectory fp = listDirectory fp >>= mapM_ removePathForcibly


fmtKernelVersion :: KernelVersion -> Text
fmtKernelVersion (major, minor, patch) =
  "" +| toInteger major |+ "." +| toInteger minor |+ "." +| toInteger patch |+ ""
