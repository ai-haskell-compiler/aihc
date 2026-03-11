module GroupByDate where

import qualified System.PosixCompat.Files as Files
import qualified System.Path.Directory as Dir
import qualified System.Path.PartClass as PartClass
import qualified System.Path as Path
import System.Path ((</>), )

import qualified Data.Time.Format as Time
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, )

import Shell.Utility.Quote as Quote
import Text.Printf (printf, )

import Control.Monad.HT ((<=<), )
import Control.Monad (when, )


folderFromStatus :: String -> Files.FileStatus -> Either String Path.AbsRelDir
folderFromStatus fmt =
   Path.parse .
   Time.formatTime Time.defaultTimeLocale fmt .
   posixSecondsToUTCTime . realToFrac .
   Files.modificationTime

parsePathIO :: (PartClass.FileDir fd) => FilePath -> IO (Path.AbsRel fd)
parsePathIO = either (ioError . userError) return . Path.parse

folderFromPath ::
   (PartClass.AbsRel ar) => String -> Path.File ar -> IO Path.AbsRelDir
folderFromPath fmt =
   parsePathIO . Time.formatTime Time.defaultTimeLocale fmt <=<
   Dir.getModificationTime


makeDstPath :: Path.File ar0 -> Path.Dir ar1 -> Path.File ar1
makeDstPath src dst = dst </> Path.takeFileName src

infixl 0 $/

($/) ::
   (PartClass.AbsRel ar, PartClass.FileDir fd) =>
   (FilePath -> a) -> (Path.Path ar fd -> a)
f $/ path = f $ Path.toString path

commandFromPath ::
   (PartClass.AbsRel ar) => Bool -> String -> String -> Path.File ar -> IO ()
commandFromPath fullDst cmd fmt src = do
   dst <- folderFromPath fmt src
   printf "mkdir -p %s && %s %s %s\n"
      (Quote.minimal $/ dst)
      cmd
      (Quote.minimal $/ src)
      (if fullDst
         then Quote.minimal $/ makeDstPath src dst
         else Quote.minimal $/ dst)

movePath :: (PartClass.AbsRel ar) => String -> Path.File ar -> IO ()
movePath fmt src = do
   dst <- folderFromPath fmt src
   Dir.createDirectoryIfMissing True dst
   Dir.renameFile src (makeDstPath src dst)

copyPath :: (PartClass.AbsRel ar) => String -> Path.File ar -> IO ()
copyPath fmt src = do
   dstDir <- folderFromPath fmt src
   Dir.createDirectoryIfMissing True dstDir
   let dst = makeDstPath src dstDir
   Dir.copyFile src dst
   status <- Files.getFileStatus $/ src
   (Files.setFileTimes $/ dst)
      (Files.accessTime status) (Files.modificationTime status)
   when False $
      -- this could fail, if the owner is 'root' e.g. for data on removeable media
      (Files.setOwnerAndGroup $/ dst)
         (Files.fileOwner status) (Files.fileGroup status)

symLinkPath :: (PartClass.AbsRel ar) => String -> Path.File ar -> IO ()
symLinkPath fmt src = do
   dst <- folderFromPath fmt src
   Dir.createDirectoryIfMissing True dst
   Files.createSymbolicLink $/ src $/ makeDstPath src dst
