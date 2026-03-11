{-# LANGUAGE CPP #-}
import           Control.Arrow ((+++))
import           Control.Monad (filterM, void)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Resource (MonadResource, runResourceT)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import           Data.List (foldl')
import qualified Data.Text as T
import           Data.Time.LocalTime (utcToLocalTime, utc)
import qualified System.Console.GetOpt as Opt
import           System.Directory (doesDirectoryExist, getModificationTime
#if MIN_VERSION_directory(1,2,6)
#if MIN_VERSION_directory(1,3,0)
  , pathIsSymbolicLink
#else
  , isSymbolicLink
#endif
  , listDirectory
#else
  , getDirectoryContents
#endif
  )
import           System.Environment (getProgName, getArgs)
import           System.Exit (exitFailure)
import           System.FilePath.Posix ((</>)) -- zip files only want forward slashes
import           System.IO (stdout, hPutStrLn, stderr)

import           Codec.Archive.Zip.Conduit.Zip

opts :: [Opt.OptDescr (ZipOptions -> ZipOptions)]
opts =
  [ Opt.Option "z" ["compress"] (Opt.ReqArg (\l o -> o{ zipOptCompressLevel = read l }) "LEVEL")
    "set compression level for files (0-9)"
  , Opt.Option "0" ["store"] (Opt.NoArg (\o -> o{ zipOptCompressLevel = 0 }))
    "don't compress files (-z0)"
  , Opt.Option "e" ["zip64"] (Opt.NoArg (\o -> o{ zipOpt64 = True }))
    "enable zip64 support for files over 4GB"
  , Opt.Option "c" ["comment"] (Opt.ReqArg (\c o -> o{ zipOptInfo = (zipOptInfo o){ zipComment = BSC.pack c }}) "TEXT")
    "set zip comment"
  ]

generate :: (MonadIO m, MonadResource m) => [FilePath] -> C.ConduitM () (ZipEntry, ZipData m) m ()
generate (p:paths) = do
  t <- liftIO $ getModificationTime p
  let e = ZipEntry
        { zipEntryName = Right $ BSC.pack $ dropWhile ('/' ==) p
        , zipEntryTime = utcToLocalTime utc t -- FIXME: timezone
        , zipEntrySize = Nothing
        , zipEntryExternalAttributes = Nothing
        }
  isd <- liftIO $ doesDirectoryExist p
  if isd
    then do
      dl <- liftIO $
#if MIN_VERSION_directory(1,2,6)
        filterM (fmap not .
#if MIN_VERSION_directory(1,3,0)
          pathIsSymbolicLink
#else
          isSymbolicLink
#endif
          ) . map (p </>) =<< listDirectory p
#else
        filter (`notElem` [".",".."]) . map (p </>) <$> getDirectoryContents p
#endif
      C.yield (e{ zipEntryName = (`T.snoc` '/') +++ (`BSC.snoc` '/') $ zipEntryName e, zipEntrySize = Just 0 }, mempty)
      generate $ dl ++ paths
    else do
      C.yield (e, zipFileData p)
      generate paths
generate [] = return ()

main :: IO ()
main = do
  prog <- getProgName
  args <- getArgs
  (opt, paths) <- case Opt.getOpt Opt.Permute opts args of
    (ol, paths@(_:_), []) -> return (foldl' (flip ($)) defaultZipOptions ol, paths)
    (_, _, err) -> do
      mapM_ (hPutStrLn stderr) err
      hPutStrLn stderr $ Opt.usageInfo ("Usage: " ++ prog ++ " [OPTION...] PATH ...\nWrite a zip file to stdout containing the given files or directories (recursively).") opts
      exitFailure
  runResourceT $ C.runConduit
    $ generate paths
    C..| void (zipStream opt)
    C..| CB.sinkHandle stdout
