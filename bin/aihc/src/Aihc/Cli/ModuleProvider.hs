-- | The facts a consumer takes from the modules of the packages it depends
-- on, one module at a time.
--
-- A unit asks for the modules it imports and nothing else, so the facts
-- of a package are read when a unit first needs them, by the task that
-- needs them, and once for every unit that asks.
module Aihc.Cli.ModuleProvider
  ( InstanceProvider,
    PackageLocator,
    PackageSource (..),
    ModuleProvider,
    ResolvedModuleFacts (..),
    TypedModuleFacts (..),
    newModuleProvider,
    providerPackagesOf,
    providerResolved,
    providerTyped,
    providerInstanceFacts,
    moduleNameDirectory,
  )
where

import Aihc.Cli.BuildStamp (ModuleDigests (..), PackageDigests (..), packageDigestsPath, readStamp)
import Aihc.Cli.ResolveArtifact (ResolveArtifact (..), decodeResolveArtifact)
import Aihc.Cli.TypeArtifact (TypeArtifact (..), decodeTypeArtifact)
import Aihc.Resolve (Exports, ModuleKey (..), Package (..), PackageId (..))
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
type PackageLocator = Map PackageId PackageSource

-- | Where the facts of the modules of a package come from: its directory
-- in the store, or the units that compile it in the same graph.
data PackageSource
  = StorePackage !FilePath
  | GraphPackage
      { graphResolved :: Text -> IO ResolvedModuleFacts,
        graphTyped :: Text -> IO TypedModuleFacts,
        -- | The instance facts the unit of the module declares itself.
        graphOwnFacts :: Text -> IO TcInterface
      }

-- | What the resolve phase of a consumer takes from a module.
data ResolvedModuleFacts = ResolvedModuleFacts
  { resolvedModuleScope :: !Exports,
    resolvedModuleScopeDigest :: !Text,
    -- | Whether the module resolved. A module from the store did.
    resolvedModuleSuccess :: !Bool
  }

-- | What the type phase of a consumer takes from a module.
data TypedModuleFacts = TypedModuleFacts
  { typedModuleInterface :: !TcInterface,
    typedModuleTypeDigest :: !Text,
    -- | The digest of the instance facts of the unit of the module, which
    -- covers every unit below it.
    typedModuleFactsDigest :: !Text,
    -- | The modules whose instances the module sees.
    typedModuleInstanceProviders :: !(Set InstanceProvider),
    -- | Whether the module type checked. A module from the store did.
    typedModuleSuccess :: !Bool
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

-- | A provider over the dependencies of a package, each with the modules
-- it exposes and where its facts come from, and the locator for every
-- package below them.
newModuleProvider :: PackageLocator -> [(Package, [Text], PackageSource)] -> IO ModuleProvider
newModuleProvider locator dependencies = do
  digestsMemo <- newMemo
  resolvedMemo <- newMemo
  typedMemo <- newMemo
  factsMemo <- newMemo
  let sources =
        Map.fromList
          [ (packageId package, source)
          | (package, _, source) <- dependencies
          ]
      modules =
        Map.fromListWith
          (flip (<>))
          [ (name, [package])
          | (package, names, _) <- dependencies,
            name <- names
          ]
      locate packageId' =
        maybe
          (ioError (userError ("The package that provides instances is not installed: " <> T.unpack (packageIdText packageId'))))
          pure
          (Map.lookup packageId' locator)
      packageSource package =
        maybe
          (ioError (userError ("The module is not from a dependency package: " <> T.unpack (packageIdText (packageId package)))))
          pure
          (Map.lookup (packageId package) sources)
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
          source <- packageSource package
          case source of
            GraphPackage {graphResolved} -> graphResolved name
            StorePackage directory -> do
              digests <- moduleDigests directory name
              let path = directory </> moduleNameDirectory name </> "resolve.cbor"
              bytes <- BS.readFile path
              artifact <- either (ioError . userError . (("Invalid resolve artifact " <> path <> ": ") <>)) pure (decodeResolveArtifact bytes)
              unless (resolveArtifactModuleName artifact == name) (ioError (userError ("Resolve artifact module name does not match " <> path)))
              evaluate (force (ResolvedModuleFacts (resolveArtifactExports artifact) (moduleScopeDigest digests) True))
      typed key@(ModuleKey package name) =
        typedMemo key $ do
          source <- packageSource package
          case source of
            GraphPackage {graphTyped} -> graphTyped name
            StorePackage directory -> do
              digests <- moduleDigests directory name
              let path = directory </> moduleNameDirectory name </> "type.cbor"
              artifact <- readTypeArtifactFile path
              unless (typeArtifactModuleName artifact == name) (ioError (userError ("Type artifact module name does not match " <> path)))
              facts <- factsArtifact (directory </> moduleFactsArtifact digests)
              let providers = Set.fromList (Map.findWithDefault [] name (typeArtifactInstanceProviders facts))
              evaluate (force (TypedModuleFacts (typeArtifactInterface artifact) (moduleTypeDigest digests) (moduleFactsDigest digests) providers True))
      instanceFacts providers = do
        let byPackage = Map.fromListWith (<>) [(packageId', [name]) | (packageId', name) <- Set.toList providers]
        interfaces <- forM (Map.toList byPackage) $ \(packageId', names) -> do
          source <- locate packageId'
          case source of
            GraphPackage {graphOwnFacts} -> mapM graphOwnFacts names
            StorePackage directory -> do
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
  rnf (ResolvedModuleFacts scope digest success) = rnf scope `seq` rnf digest `seq` rnf success

instance NFData TypedModuleFacts where
  rnf (TypedModuleFacts interface typeDigest factsDigest providers success) =
    rnf interface `seq` rnf typeDigest `seq` rnf factsDigest `seq` rnf providers `seq` rnf success
