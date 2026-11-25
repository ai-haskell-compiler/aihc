{-# LANGUAGE OverloadedStrings #-}

-- | Read the core libraries of a store into a playground bundle.
--
-- The bundle takes the newest @aihc-base@ entry of the store and the
-- @aihc-prim@ entry it depends on: the packages a program sees without
-- a dependency of its own. When the store has an
-- @aihc-template-haskell@ entry above that @aihc-base@ entry, the bundle
-- also takes it and its @aihc-internal@ entry. Then a program can import
-- the Template Haskell library. See "Aihc.Cli.PlayBundle" for the format.
module Aihc.Cli.PlayBundleStore
  ( writeStoreBundle,
    storeBundle,
    StoreBundleInfo (..),
    writeStoreRuntime,
  )
where

import Aihc.Cli.PackageManifest (PackageManifest (..), packageManifestPath, readPackageManifest)
import Aihc.Cli.PlayBundle (BundleModule (..), encodeBundle)
import Aihc.Cli.TypeArtifact (TypeArtifact (..), decodeTypeArtifact)
import Aihc.Resolve (Package (..), PackageId (..))
import Control.Monad (filterM, forM, unless)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getModificationTime, listDirectory)
import System.FilePath (joinPath, takeExtension, (</>))

-- | What a bundle was built from.
data StoreBundleInfo = StoreBundleInfo
  { -- | The store entries the bundle holds, as directory names.
    storeBundlePackages :: ![Text],
    storeBundleModules :: !Int
  }
  deriving (Show)

-- | The packages of the bundle, by name, in dependency order.
corePackages :: [Text]
corePackages = ["aihc-prim", "aihc-base", "aihc-internal", "aihc-template-haskell"]

-- | Write the bundle of a store to a file.
writeStoreBundle :: FilePath -> FilePath -> IO StoreBundleInfo
writeStoreBundle store output = do
  (info, bytes) <- storeBundle store
  BL.writeFile output bytes
  pure info

-- | The bundle of a store. The store is the platform directory with the
-- package entries, or its parent.
storeBundle :: FilePath -> IO (StoreBundleInfo, BL.ByteString)
storeBundle store = do
  platform <- findPlatformDirectory store
  entries <- coreEntries (const True) platform
  let ordered = [entry | name <- corePackages, entry <- Map.elems entries, packageManifestName (snd entry) == name]
  -- The facts name a package by its unit identity, not by the store
  -- directory, which also carries the fingerprint of the entry.
  prim <- case [packageManifestUnitId manifest | (_, manifest) <- ordered, packageManifestName manifest == "aihc-prim"] of
    [unitId] -> pure (PackageId unitId)
    _ -> ioError (userError "the store has no aihc-prim entry below aihc-base")
  modules <- fmap concat . forM ordered $ \(identity, manifest) -> do
    let package = Package (packageManifestName manifest) (PackageId (packageManifestUnitId manifest))
        directory = platform </> identity
    forM (packageManifestModules manifest) $ \name -> do
      resolve <- BS.readFile (directory </> moduleNameDirectory name </> "resolve.cbor")
      pure (BundleModule package name resolve)
  interfaces <- fmap concat . forM ordered $ \(identity, manifest) -> do
    let directory = platform </> identity
    forM (packageManifestCompiledModules manifest) $ \name -> do
      let path = directory </> moduleNameDirectory name </> "type.cbor"
      bytes <- BL.readFile path
      artifact <- either (ioError . userError . (("Invalid type artifact " <> path <> ": ") <>)) pure (decodeTypeArtifact bytes)
      pure (typeArtifactInterface artifact)
  let info = StoreBundleInfo {storeBundlePackages = map (T.pack . fst) ordered, storeBundleModules = length modules}
  pure (info, encodeBundle prim modules interfaces)

-- | The directory with the package entries: the given one, or the child
-- that holds an @aihc-base@ entry.
findPlatformDirectory :: FilePath -> IO FilePath
findPlatformDirectory store = do
  direct <- hasEntry store
  if direct
    then pure store
    else do
      children <- listDirectory store
      candidates <- filterM (hasEntry . (store </>)) children
      case candidates of
        [child] -> pure (store </> child)
        [] -> ioError (userError ("no aihc-base entry under " <> store))
        _ -> ioError (userError ("several platform directories under " <> store <> ": give one of them"))
  where
    hasEntry directory = do
      isDirectory <- doesDirectoryExist directory
      if not isDirectory
        then pure False
        else any ("aihc-base-" `isPrefixOfPath`) <$> listDirectory directory
    isPrefixOfPath prefix path = take (length prefix) path == prefix

-- | The core entries of a bundle, keyed by identity: the newest
-- @aihc-base@ entry that matches, and the core packages below it. The
-- result also has the newest @aihc-template-haskell@ entry that matches
-- and that uses the same @aihc-base@ entry, with the core packages below
-- it. A store without such an entry gives no Template Haskell packages.
coreEntries :: (PackageManifest -> Bool) -> FilePath -> IO (Map.Map FilePath (FilePath, PackageManifest))
coreEntries matches platform = do
  base <- newestEntryMatching matches platform "aihc-base"
  baseEntries <- closure platform Map.empty [base]
  candidates <- entriesMatching matches platform "aihc-template-haskell"
  let sameBase found = [identity | (identity, manifest) <- Map.elems found, packageManifestName manifest == "aihc-base"] == [fst base]
      firstWithBase [] = pure baseEntries
      firstWithBase (candidate : rest) = do
        found <- closure platform baseEntries [candidate]
        if sameBase found then pure found else firstWithBase rest
  firstWithBase candidates

-- | The newest store entry of a package that matches, by the time of its
-- manifest.
newestEntryMatching :: (PackageManifest -> Bool) -> FilePath -> Text -> IO (FilePath, PackageManifest)
newestEntryMatching matches platform name = do
  candidates <- entriesMatching matches platform name
  case candidates of
    entry : _ -> pure entry
    [] -> ioError (userError ("no " <> T.unpack name <> " entry under " <> platform))

-- | The store entries of a package that match, the newest first, by the
-- time of their manifests.
entriesMatching :: (PackageManifest -> Bool) -> FilePath -> Text -> IO [(FilePath, PackageManifest)]
entriesMatching matches platform name = do
  children <- listDirectory platform
  candidates <- fmap concat . forM children $ \child -> do
    let manifestPath = packageManifestPath (platform </> child)
    present <- doesFileExist manifestPath
    if not present
      then pure []
      else do
        manifest <- readPackageManifest manifestPath
        case manifest of
          Right value
            | packageManifestName value == name,
              matches value -> do
                time <- getModificationTime manifestPath
                pure [(time, (child, value))]
          _ -> pure []
  pure (map snd (sortOn (Down . fst) candidates))

-- | The core packages below the given entries, keyed by identity.
closure :: FilePath -> Map.Map FilePath (FilePath, PackageManifest) -> [(FilePath, PackageManifest)] -> IO (Map.Map FilePath (FilePath, PackageManifest))
closure _ known [] = pure known
closure platform known ((identity, manifest) : rest)
  | Map.member identity known = closure platform known rest
  | otherwise = do
      dependencies <- fmap concat . forM (packageManifestDependencies manifest) $ \dependency -> do
        let manifestPath = packageManifestPath (platform </> T.unpack dependency)
        present <- doesFileExist manifestPath
        unless present $
          ioError (userError ("the dependency " <> T.unpack dependency <> " of " <> identity <> " is not in the store"))
        value <- readPackageManifest manifestPath >>= either (ioError . userError) pure
        pure [(T.unpack dependency, value) | packageManifestName value `elem` ("aihc-rts" : corePackages)]
      closure platform (Map.insert identity (identity, manifest) known) (dependencies <> rest)

-- | The directory of a module inside a store entry, as the store lays it
-- out: one directory level for each part of the name.
moduleNameDirectory :: Text -> FilePath
moduleNameDirectory = joinPath . map T.unpack . T.splitOn "."

-- | Copy the core archives and C objects for the browser linker.
writeStoreRuntime :: FilePath -> FilePath -> IO ()
writeStoreRuntime store output = do
  platform <- findPlatformDirectory store
  entries <- coreEntries (not . any (`elem` ["lto", "no-code"]) . packageManifestFlags) platform
  createDirectoryIfMissing True output
  files <- fmap concat . forM (Map.toAscList entries) $ \(identity, (_, manifest)) -> do
    let root = platform </> identity
        cbits = root </> "cbits"
    present <- doesDirectoryExist cbits
    objects <- if present then sortOn id . filter ((== ".o") . takeExtension) <$> listDirectory cbits else pure []
    let archive = "lib" <> T.unpack (packageManifestName manifest) <> ".a"
        paths = [(name, cbits </> name) | name <- objects] <> [(archive, root </> "lib" </> archive) | packageManifestName manifest `elem` corePackages]
    forM paths $ \(name, path) -> do
      let target = identity <> "-" <> name
      copyFile path (output </> target)
      pure target
  writeFile (output </> "files.list") (unlines files)
