module Aihc.Cli.ArtifactCache
  ( hashChunks,
    compilerBuildIdentity,
    executableIdentity,
    sourceFilesHash,
  )
where

import Control.Exception (IOException, evaluate, try)
import Control.Monad (forM)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BL
import Data.List (nub, sort)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Numeric (showHex)
import System.Directory (XdgDirectory (XdgCache), canonicalizePath, createDirectoryIfMissing, findExecutable, getFileSize, getModificationTime, getXdgDirectory, renameFile)
import System.Environment (getExecutablePath)
import System.FilePath (makeRelative, (<.>), (</>))

-- Each field has a length prefix to prevent ambiguous concatenation.
hashChunks :: [BS.ByteString] -> String
hashChunks = concatMap hex . BS.unpack . SHA256.hashlazy . BL.fromChunks . concatMap field
  where
    field bytes = [BS8.pack (show (BS.length bytes) <> ":"), bytes]
    hex byte = let value = showHex byte "" in replicate (2 - length value) '0' <> value

-- | The identity of the running compiler: the hash of its executable.
-- Two compilers that differ in any byte get different identities, so the
-- store never mixes the artifacts of two compilers. The identity does not
-- depend on the compiler that built this one or on a build tool.
--
-- A hash of the whole executable takes a fraction of a second. A cache file
-- keeps it for each path, size, and modification time of the executable.
compilerBuildIdentity :: IO String
compilerBuildIdentity = do
  path <- canonicalizePath =<< getExecutablePath
  size <- getFileSize path
  modified <- getModificationTime path
  cacheDirectory <- getXdgDirectory XdgCache ("aihc" </> "compiler-identity")
  let cachePath = cacheDirectory </> hashChunks (map (TE.encodeUtf8 . T.pack) [path, show size, show modified])
  cached <- try (BS.readFile cachePath) :: IO (Either IOException BS.ByteString)
  case cached of
    Right identity | BS.length identity == 64 -> pure (BS8.unpack identity)
    _ -> do
      contents <- BL.readFile path
      identity <- evaluate (hashChunks (BL.toChunks contents))
      -- A failed write only costs the next run the same hash again.
      _ <- try (writeCache cacheDirectory cachePath identity) :: IO (Either IOException ())
      pure identity
  where
    -- Write a whole file and then rename it, so a reader never sees a part.
    writeCache cacheDirectory cachePath identity = do
      createDirectoryIfMissing True cacheDirectory
      let temporary = cachePath <.> "tmp"
      BS.writeFile temporary (BS8.pack identity)
      renameFile temporary cachePath

executableIdentity :: FilePath -> IO String
executableIdentity command = do
  found <- findExecutable command
  path <- maybe (ioError (userError ("Compiler tool is absent: " <> command))) canonicalizePath found
  pure (hashChunks [TE.encodeUtf8 (T.pack path)])

sourceFilesHash :: FilePath -> [FilePath] -> IO String
sourceFilesHash root files = do
  chunks <- forM (sort (nub files)) $ \path -> do
    bytes <- BS.readFile path
    digest <- evaluate (SHA256.hash bytes)
    pure [TE.encodeUtf8 (T.pack (makeRelative root path)), digest]
  pure (hashChunks (concat chunks))
