-- | The facts a consumer takes from the modules of the packages it depends
-- on, one module at a time.
--
-- A unit asks for the modules it imports and nothing else, so the facts
-- of a package are read when a unit first needs them, by the task that
-- needs them, and once for every unit that asks.
module Aihc.Cli.ModuleProvider
  ( InstanceProvider,
    PackageLocator,
    ModuleProvider,
    ResolvedModuleFacts (..),
    TypedModuleFacts (..),
    newStoreModuleProvider,
    providerPackagesOf,
    providerResolved,
    providerTyped,
    providerInstanceFacts,
    moduleNameDirectory,
  )
where

import Aihc.Cli.BuildStamp (ModuleDigests (..), PackageDigests (..), packageDigestsPath, readStamp)
import Aihc.Cli.PackageManifest (PackageManifest (..))
import Aihc.Cli.ResolveArtifact (ResolveArtifact (..), decodeResolveArtifact)
import Aihc.Cli.TypeArtifact (TypeArtifact (..), decodeTypeArtifact)
import Aihc.Resolve (ModuleKey (..), Package (..), PackageId (..), Scope)
import Aihc.Tc (MergeCheck (..), TcInterface, mergeTcInterfaces)
import Control.Concurrent.STM (TMVar, TVar, atomically, newEmptyTMVar, newTVarIO, putTMVar, readTMVar, readTVar, writeTVar)
import Control.DeepSeq (NFData (..), force)
import Control.Exception (SomeException, evaluate, throwIO, try)
import Control.Monad (forM, unless)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.List (nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import System.FilePath ((</>))

-- | The package and module that declare an instance.
type InstanceProvider = (PackageId, Text)

-- | Where the package of each identity is. The instances a module sees can
-- come from a package below the dependencies of its own package.
type PackageLocator = Map PackageId FilePath

-- | What the resolve phase of a consumer takes from a module.
data ResolvedModuleFacts = ResolvedModuleFacts
  { resolvedModuleScope :: !Scope,
    resolvedModuleScopeDigest :: !Text
  }

-- | What the type phase of a consumer takes from a module.
data TypedModuleFacts = TypedModuleFacts
  { typedModuleInterface :: !TcInterface,
    typedModuleTypeDigest :: !Text,
    -- | The digest of the instance facts of the unit of the module, which
    -- covers every unit below it.
    typedModuleFactsDigest :: !Text,
    -- | The modules whose instances the module sees.
    typedModuleInstanceProviders :: !(Set InstanceProvider)
  }

-- | The modules of the dependency packages, by name, and their facts on
-- request.
data ModuleProvider = ModuleProvider
  { providerModules :: !(Map Text [Package]),
    providerResolved :: ModuleKey -> IO ResolvedModuleFacts,
    providerTyped :: ModuleKey -> IO TypedModuleFacts,
    -- | The instance facts of the given providers, merged as a unit merges
    -- the facts of the units below it.
    providerInstanceFacts :: Set InstanceProvider -> IO TcInterface
  }

-- | The dependency packages that expose a module of the name.
providerPackagesOf :: ModuleProvider -> Text -> [Package]
providerPackagesOf provider name = Map.findWithDefault [] name (providerModules provider)

-- | A provider over installed packages: the dependencies, each with its
-- manifest and directory, and the locator for every package below them.
newStoreModuleProvider :: PackageLocator -> [(PackageManifest, FilePath)] -> IO ModuleProvider
newStoreModuleProvider locator dependencies = do
  digestsMemo <- newMemo
  resolvedMemo <- newMemo
  typedMemo <- newMemo
  factsMemo <- newMemo
  let directories =
        Map.fromList
          [ (PackageId (packageManifestUnitId manifest), directory)
          | (manifest, directory) <- dependencies
          ]
      modules =
        Map.fromListWith
          (flip (<>))
          [ (name, [Package (packageManifestName manifest) (PackageId (packageManifestUnitId manifest))])
          | (manifest, _) <- dependencies,
            name <- packageManifestModules manifest
          ]
      locate packageId =
        maybe
          (ioError (userError ("The package that provides instances is not installed: " <> T.unpack (packageIdText packageId))))
          pure
          (Map.lookup packageId locator)
      packageDirectory package =
        maybe
          (ioError (userError ("The module is not from a dependency package: " <> T.unpack (packageIdText (packageId package)))))
          pure
          (Map.lookup (packageId package) directories)
      packageDigests directory =
        digestsMemo directory $
          readStamp (packageDigestsPath directory)
            >>= maybe (ioError (userError ("The installed package has no digests: " <> directory))) pure
      moduleDigests directory name = do
        digests <- packageDigests directory
        maybe
          (ioError (userError ("The installed package has no digests for module " <> T.unpack name <> ": " <> directory)))
          pure
          (Map.lookup name (packageDigestsModules digests))
      factsArtifact path =
        factsMemo path $ do
          artifact <- readTypeArtifactFile path
          _ <- evaluate (force (typeArtifactInterface artifact, typeArtifactInstanceProviders artifact))
          pure artifact
      resolved key@(ModuleKey package name) =
        resolvedMemo key $ do
          directory <- packageDirectory package
          digests <- moduleDigests directory name
          let path = directory </> moduleNameDirectory name </> "resolve.cbor"
          bytes <- BS.readFile path
          artifact <- either (ioError . userError . (("Invalid resolve artifact " <> path <> ": ") <>)) pure (decodeResolveArtifact bytes)
          unless (resolveArtifactModuleName artifact == name) (ioError (userError ("Resolve artifact module name does not match " <> path)))
          evaluate (force (ResolvedModuleFacts (resolveArtifactScope artifact) (moduleScopeDigest digests)))
      typed key@(ModuleKey package name) =
        typedMemo key $ do
          directory <- packageDirectory package
          digests <- moduleDigests directory name
          let path = directory </> moduleNameDirectory name </> "type.cbor"
          artifact <- readTypeArtifactFile path
          unless (typeArtifactModuleName artifact == name) (ioError (userError ("Type artifact module name does not match " <> path)))
          facts <- factsArtifact (directory </> moduleFactsArtifact digests)
          let providers = Set.fromList (Map.findWithDefault [] name (typeArtifactInstanceProviders facts))
          evaluate (force (TypedModuleFacts (typeArtifactInterface artifact) (moduleTypeDigest digests) (moduleFactsDigest digests) providers))
      instanceFacts providers = do
        let byPackage = Map.fromListWith (<>) [(packageId', [name]) | (packageId', name) <- Set.toList providers]
        interfaces <- forM (Map.toList byPackage) $ \(packageId', names) -> do
          directory <- locate packageId'
          digests <- packageDigests directory
          let paths = nub [moduleFactsArtifact entry | name <- names, Just entry <- [Map.lookup name (packageDigestsModules digests)]]
          forM paths $ \path -> typeArtifactInterface <$> factsArtifact (directory </> path)
        pure (mergeTcInterfaces TrustMergedFacts (concat interfaces))
  pure
    ModuleProvider
      { providerModules = modules,
        providerResolved = resolved,
        providerTyped = typed,
        providerInstanceFacts = instanceFacts
      }

readTypeArtifactFile :: FilePath -> IO TypeArtifact
readTypeArtifactFile path = do
  bytes <- BL.readFile path
  either (ioError . userError . (("Invalid type artifact " <> path <> ": ") <>)) pure (decodeTypeArtifact bytes)

-- | A table that runs the action of a key once, however many tasks ask
-- for it at the same time. The first to ask runs the action; the others
-- wait for its result, and get its exception if it has one.
newMemo :: (Ord key) => IO (key -> IO value -> IO value)
newMemo = do
  table <- newTVarIO Map.empty
  pure (memoize table)

memoize :: (Ord key) => TVar (Map key (TMVar (Either SomeException value))) -> key -> IO value -> IO value
memoize table key action = do
  (slot, owner) <- atomically $ do
    entries <- readTVar table
    case Map.lookup key entries of
      Just slot -> pure (slot, False)
      Nothing -> do
        slot <- newEmptyTMVar
        writeTVar table (Map.insert key slot entries)
        pure (slot, True)
  if owner
    then do
      result <- try action
      atomically (putTMVar slot result)
      either throwIO pure result
    else atomically (readTMVar slot) >>= either throwIO pure

moduleNameDirectory :: Text -> FilePath
moduleNameDirectory = foldl (</>) "" . map T.unpack . T.splitOn "."

instance NFData ResolvedModuleFacts where
  rnf (ResolvedModuleFacts scope digest) = rnf scope `seq` rnf digest

instance NFData TypedModuleFacts where
  rnf (TypedModuleFacts interface typeDigest factsDigest providers) =
    rnf interface `seq` rnf typeDigest `seq` rnf factsDigest `seq` rnf providers
