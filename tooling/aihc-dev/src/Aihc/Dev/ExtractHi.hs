-- | Extract scoping and typing information from GHC .hi interface files.
--
-- Given a package name, this module locates the package via @ghc-pkg@,
-- reads every exposed module's .hi file using the GHC API, and produces
-- a structured 'PackageInterface' suitable for YAML serialization.
--
-- Handles re-export facades (e.g. @base@ re-exporting from @ghc-internal@)
-- by following exported names to their defining modules.
module Aihc.Dev.ExtractHi
  ( extractPackage,
  )
where

import Aihc.Dev.ExtractHi.GhcSession (withReadIface)
import Aihc.Dev.ExtractHi.Types
import Control.Exception (IOException, catch)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import GHC (Ghc)
import GHC.Iface.Syntax
  ( IfaceClassBody (..),
    IfaceClassOp (..),
    IfaceConDecl (..),
    IfaceConDecls (..),
    IfaceDecl (..),
  )
import GHC.Iface.Type (IfaceTyConBinder, IfaceType, ShowForAllFlag (..), pprIfaceSigmaType, pprIfaceType)
import GHC.Types.Avail (AvailInfo (..))
import GHC.Types.Name (Name, getOccString, nameModule_maybe)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Unit.Module (moduleNameString)
import GHC.Unit.Module.ModIface (ModIface, mi_decls, mi_exports, mi_fixities)
import GHC.Unit.Types (moduleName, moduleUnit, unitString)
import GHC.Utils.Outputable (showSDocUnsafe)
import Language.Haskell.Syntax.Basic qualified as GHC
import System.Directory (doesFileExist)
import System.FilePath ((<.>), (</>))
import System.Process (readProcess)

-- | Cached module data: declarations and fixities from a single .hi file.
data CachedModule = CachedModule
  { cmDecls :: Map String IfaceDecl,
    cmFixities :: Map String GHC.Fixity
  }

-- | Extract the full interface for a locally installed package.
extractPackage :: String -> IO PackageInterface
extractPackage pkgName = do
  (pkgId, importDir, exposedMods) <- queryPackage pkgName
  withReadIface $ \readIface -> do
    cacheRef <- liftIO $ newIORef (Map.empty :: Map (String, String) CachedModule)
    modules <- mapM (extractSingleModule readIface cacheRef importDir) exposedMods
    pure
      PackageInterface
        { piPackage = T.pack pkgId,
          piModules = modules
        }

-- | Load a remote module's data into the cache, returning the cached data.
loadModule ::
  (FilePath -> IO ModIface) ->
  IORef (Map (String, String) CachedModule) ->
  String ->
  String ->
  IO CachedModule
loadModule readIface cacheRef unit modName = do
  cache <- readIORef cacheRef
  let key = (unit, modName)
  case Map.lookup key cache of
    Just cm -> pure cm
    Nothing -> do
      mImportDir <- queryImportDir unit
      cm <- case mImportDir of
        Nothing -> pure emptyCachedModule
        Just importDir -> do
          let hiPath = importDir </> map dotToSlash modName <.> "hi"
          exists <- doesFileExist hiPath
          if exists
            then do
              iface <- readIface hiPath
              let ds = map snd (mi_decls iface)
                  decls = Map.fromList [(getOccString (ifName d), d) | d <- ds]
                  fixs = Map.fromList [(occNameString occ, f) | (occ, f) <- mi_fixities iface]
              pure CachedModule {cmDecls = decls, cmFixities = fixs}
            else pure emptyCachedModule
      modifyIORef' cacheRef (Map.insert key cm)
      pure cm

emptyCachedModule :: CachedModule
emptyCachedModule = CachedModule Map.empty Map.empty

-- | Look up a declaration by following a Name to its defining module.
lookupDecl ::
  (FilePath -> IO ModIface) ->
  IORef (Map (String, String) CachedModule) ->
  Name ->
  IO (Maybe IfaceDecl)
lookupDecl readIface cacheRef name = case nameModule_maybe name of
  Nothing -> pure Nothing
  Just m -> do
    let unit = unitString (moduleUnit m)
        modName = moduleNameString (moduleName m)
    cm <- loadModule readIface cacheRef unit modName
    pure (Map.lookup (getOccString name) (cmDecls cm))

-- | Look up a fixity by following a Name to its defining module.
lookupFixity ::
  (FilePath -> IO ModIface) ->
  IORef (Map (String, String) CachedModule) ->
  Name ->
  IO (Maybe GHC.Fixity)
lookupFixity readIface cacheRef name = case nameModule_maybe name of
  Nothing -> pure Nothing
  Just m -> do
    let unit = unitString (moduleUnit m)
        modName = moduleNameString (moduleName m)
    cm <- loadModule readIface cacheRef unit modName
    pure (Map.lookup (getOccString name) (cmFixities cm))

-- | Extract interface information from a single module.
extractSingleModule ::
  (FilePath -> IO ModIface) ->
  IORef (Map (String, String) CachedModule) ->
  FilePath ->
  String ->
  Ghc ModuleInterface
extractSingleModule readIface cacheRef importDir modName = do
  let hiPath = importDir </> map dotToSlash modName <.> "hi"
  exists <- liftIO $ doesFileExist hiPath
  if exists
    then do
      iface <- liftIO $ readIface hiPath
      liftIO $ ifaceToModule readIface cacheRef modName iface
    else pure emptyModule
  where
    emptyModule =
      ModuleInterface
        { miModule = T.pack modName,
          miTypes = [],
          miValues = [],
          miClasses = [],
          miFixities = []
        }

-- | Convert a 'ModIface' to our output representation.
ifaceToModule ::
  (FilePath -> IO ModIface) ->
  IORef (Map (String, String) CachedModule) ->
  String ->
  ModIface ->
  IO ModuleInterface
ifaceToModule readIface cacheRef modName iface = do
  let exports = mi_exports iface
      localFixities = mi_fixities iface
      localDecls = map snd (mi_decls iface)
      localDeclMap = Map.fromList [(getOccString (ifName d), d) | d <- localDecls]
      localFixMap = Map.fromList [(occNameString occ, f) | (occ, f) <- localFixities]

  -- Classify exports
  (types, values, classes) <- classifyExports exports localDeclMap (lookupDecl readIface cacheRef)

  -- Collect fixities: local ones first, then look up re-exported names
  let allExportedNames = concatMap availNames exports
  remoteFixities <- collectFixities readIface cacheRef localFixMap allExportedNames

  pure
    ModuleInterface
      { miModule = T.pack modName,
        miTypes = types,
        miValues = values,
        miClasses = classes,
        miFixities = remoteFixities
      }

-- | Get all names from an AvailInfo.
availNames :: AvailInfo -> [Name]
availNames (Avail n) = [n]
availNames (AvailTC _ subs) = subs

-- | Collect fixities for all exported names.
--
-- First checks local fixities, then follows re-exports to defining modules.
-- Only includes names that actually have a non-default fixity.
collectFixities ::
  (FilePath -> IO ModIface) ->
  IORef (Map (String, String) CachedModule) ->
  Map String GHC.Fixity ->
  [Name] ->
  IO [FixityInfo]
collectFixities readIface cacheRef localFixMap names = do
  results <- mapM lookupOne names
  pure (concat results)
  where
    lookupOne name = do
      let occStr = getOccString name
      case Map.lookup occStr localFixMap of
        Just fix -> pure [toFixityInfo occStr fix]
        Nothing -> do
          mFix <- lookupFixity readIface cacheRef name
          case mFix of
            Just fix -> pure [toFixityInfo occStr fix]
            Nothing -> pure []

    toFixityInfo nameStr (GHC.Fixity prec dir) =
      FixityInfo
        { fiName = T.pack nameStr,
          fiDirection = convertDir dir,
          fiPrecedence = prec
        }

    convertDir GHC.InfixL = InfixL
    convertDir GHC.InfixR = InfixR
    convertDir GHC.InfixN = InfixN

-- | Classify exports into types, values, and classes.
classifyExports ::
  [AvailInfo] ->
  Map String IfaceDecl ->
  (Name -> IO (Maybe IfaceDecl)) ->
  IO ([ExportedType], [ExportedValue], [ExportedClass])
classifyExports avails localDeclMap cachedLookup = do
  results <- mapM (classifySingle localDeclMap cachedLookup) avails
  let (ts, vs, cs) = foldr merge ([], [], []) results
  pure (ts, vs, cs)
  where
    merge (t, v, c) (ts, vs, cs) = (t ++ ts, v ++ vs, c ++ cs)

-- | Classify a single AvailInfo.
classifySingle ::
  Map String IfaceDecl ->
  (Name -> IO (Maybe IfaceDecl)) ->
  AvailInfo ->
  IO ([ExportedType], [ExportedValue], [ExportedClass])
classifySingle localDeclMap cachedLookup avail = case avail of
  Avail name -> do
    let nameStr = getOccString name
    mDecl <- resolveDecl localDeclMap cachedLookup nameStr name
    case mDecl of
      Just decl -> pure ([], [extractValue decl], [])
      Nothing ->
        pure
          ( [],
            [ ExportedValue
                { evName = T.pack nameStr,
                  evType = "<unresolved>"
                }
            ],
            []
          )
  AvailTC name subs -> do
    let nameStr = getOccString name
        subNames = map getOccString subs
    mDecl <- resolveDecl localDeclMap cachedLookup nameStr name
    case mDecl of
      Just decl@IfaceClass {} ->
        pure ([], [], [extractClass decl subNames])
      Just decl@IfaceData {} ->
        pure ([extractDataType decl subNames], [], [])
      Just decl@IfaceSynonym {} ->
        pure ([extractSynonym decl], [], [])
      Just decl@IfaceFamily {} ->
        pure ([extractFamily decl], [], [])
      _ ->
        pure
          ( [ ExportedType
                { etName = T.pack nameStr,
                  etKind = "<unresolved>",
                  etConstructors = map T.pack (filter (/= nameStr) subNames)
                }
            ],
            [],
            []
          )

-- | Resolve a declaration: check local first, then follow re-exports.
resolveDecl ::
  Map String IfaceDecl ->
  (Name -> IO (Maybe IfaceDecl)) ->
  String ->
  Name ->
  IO (Maybe IfaceDecl)
resolveDecl localDeclMap cachedLookup nameStr name =
  case Map.lookup nameStr localDeclMap of
    Just decl -> pure (Just decl)
    Nothing -> cachedLookup name

-- | Extract value/function information from an IfaceId declaration.
extractValue :: IfaceDecl -> ExportedValue
extractValue decl = case decl of
  IfaceId {ifName, ifType} ->
    ExportedValue
      { evName = T.pack (getOccString ifName),
        evType = renderType ifType
      }
  other ->
    ExportedValue
      { evName = T.pack (getOccString (ifName other)),
        evType = "<unexpected-decl-kind>"
      }

-- | Extract data type information.
extractDataType :: IfaceDecl -> [String] -> ExportedType
extractDataType decl subNames = case decl of
  IfaceData {ifName, ifBinders, ifResKind, ifCons} ->
    let parentName = getOccString ifName
        ctors = case ifCons of
          IfAbstractTyCon -> []
          IfDataTyCon _ cs -> map (T.pack . getOccString . ifConName) cs
          IfNewTyCon c -> [T.pack (getOccString (ifConName c))]
        ctorStrings = map T.unpack ctors
        extraSubs =
          [ T.pack s
          | s <- subNames,
            s /= parentName,
            s `notElem` ctorStrings
          ]
     in ExportedType
          { etName = T.pack parentName,
            etKind = renderKind ifBinders ifResKind,
            etConstructors = ctors ++ extraSubs
          }
  other ->
    ExportedType
      { etName = T.pack (getOccString (ifName other)),
        etKind = "<unexpected>",
        etConstructors = []
      }

-- | Extract type synonym information.
extractSynonym :: IfaceDecl -> ExportedType
extractSynonym decl = case decl of
  IfaceSynonym {ifName, ifBinders, ifResKind} ->
    ExportedType
      { etName = T.pack (getOccString ifName),
        etKind = renderKind ifBinders ifResKind,
        etConstructors = []
      }
  other ->
    ExportedType
      { etName = T.pack (getOccString (ifName other)),
        etKind = "<unexpected>",
        etConstructors = []
      }

-- | Extract type family information.
extractFamily :: IfaceDecl -> ExportedType
extractFamily decl = case decl of
  IfaceFamily {ifName, ifBinders, ifResKind} ->
    ExportedType
      { etName = T.pack (getOccString ifName),
        etKind = renderKind ifBinders ifResKind,
        etConstructors = []
      }
  other ->
    ExportedType
      { etName = T.pack (getOccString (ifName other)),
        etKind = "<unexpected>",
        etConstructors = []
      }

-- | Extract class information.
extractClass :: IfaceDecl -> [String] -> ExportedClass
extractClass decl _subNames = case decl of
  IfaceClass {ifName, ifBody} ->
    let methods = case ifBody of
          IfAbstractClass -> []
          IfConcreteClass {ifSigs} ->
            [extractClassOp op | op <- ifSigs]
     in ExportedClass
          { ecName = T.pack (getOccString ifName),
            ecMethods = methods
          }
  other ->
    ExportedClass
      { ecName = T.pack (getOccString (ifName other)),
        ecMethods = []
      }

-- | Extract a class method from its IfaceClassOp.
extractClassOp :: IfaceClassOp -> ClassMethod
extractClassOp (IfaceClassOp name ty _defMeth) =
  ClassMethod
    { cmName = T.pack (getOccString name),
      cmType = renderType ty
    }

-- | Render an IfaceType to a human-readable string.
renderType :: IfaceType -> Text
renderType ty = T.pack (showSDocUnsafe (pprIfaceSigmaType ShowForAllWhen ty))

-- | Render a kind from binders and result kind.
renderKind :: [IfaceTyConBinder] -> IfaceType -> Text
renderKind _binders resKind =
  T.pack (showSDocUnsafe (pprIfaceType resKind))

-- | Query @ghc-pkg@ for package information.
--
-- Returns (package-id, import-dir, [exposed-module-names]).
queryPackage :: String -> IO (String, FilePath, [String])
queryPackage pkgName = do
  pkgId <- trim <$> readProcess "ghc-pkg" ["field", pkgName, "id", "--simple-output"] ""
  importDir <- trim <$> readProcess "ghc-pkg" ["field", pkgName, "import-dirs", "--simple-output"] ""
  exposedModsRaw <- readProcess "ghc-pkg" ["field", pkgName, "exposed-modules", "--simple-output"] ""
  let exposedMods = map stripComma (words exposedModsRaw)
  pure (pkgId, importDir, exposedMods)

-- | Query @ghc-pkg@ for a package's import directory by unit id.
queryImportDir :: String -> IO (Maybe FilePath)
queryImportDir unitId = do
  result <- tryReadProcess "ghc-pkg" ["field", unitId, "import-dirs", "--simple-output"]
  case result of
    Just dir -> pure (Just (trim dir))
    Nothing -> pure Nothing

-- | Try to run a process, returning Nothing on failure.
tryReadProcess :: FilePath -> [String] -> IO (Maybe String)
tryReadProcess prog args =
  (Just <$> readProcess prog args "")
    `catch` (\(_ :: IOException) -> pure Nothing)

-- | Strip trailing commas from ghc-pkg output tokens.
stripComma :: String -> String
stripComma s = case reverse s of
  ',' : rest -> reverse rest
  _ -> s

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
  where
    isSpace c = c == ' ' || c == '\n' || c == '\r' || c == '\t'

dotToSlash :: Char -> Char
dotToSlash '.' = '/'
dotToSlash c = c
