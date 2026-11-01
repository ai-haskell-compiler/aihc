-- | Extract package interfaces with direct calls to GHC.
module Aihc.Dev.ExtractHi
  ( extractPackage,
    extractSourcePackage,
  )
where

import Aihc.Cabal (Component (..), ComponentKind (..), LibraryTarget (..), Package, exposedModules, packageComponents, unconditional)
import Aihc.Dev.ExtractHi.Ghc (GhcInterface (..), readGhcBuiltins, readGhcInterface, readGhcModule, unqualify)
import Aihc.Dev.ExtractHi.Types
import Aihc.Hackage.Package (parsePackageDescription)
import Aihc.Hackage.Util (moduleNameFilePath)
import Aihc.Parser (ParserConfig (..), defaultConfig, parseModule)
import Aihc.Parser.Pretty (prettyType)
import Aihc.Parser.Syntax
  ( BinderHead (..),
    ClassDecl (..),
    ClassDeclItem (..),
    DataConDecl (..),
    DataDecl (..),
    Decl (..),
    ExportSpec (..),
    FieldDecl (..),
    FixityAssoc,
    ForeignDecl (..),
    ForeignDirection (..),
    GadtBody (..),
    IEBundledMember (..),
    IEEntityNamespace (..),
    Module (..),
    NewtypeDecl (..),
    Pattern (..),
    Type (..),
    TypeFamilyDecl (..),
    TypeSynDecl (..),
    UnqualifiedName,
    ValueDecl (..),
    moduleExports,
    peelClassDeclItemAnn,
    peelDataConAnn,
    peelDeclAnn,
    renderName,
    renderUnqualifiedName,
  )
import Aihc.Parser.Syntax qualified as Syntax
import Control.Applicative ((<|>))
import Data.ByteString qualified as BS
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.String (renderString)
import System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory, listDirectory)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, takeExtension, (<.>), (</>))
import System.Process (readProcess, readProcessWithExitCode)

-- | Extract the interface of an installed package with direct GHC calls.
extractPackage :: String -> IO PackageInterface
extractPackage pkgName = do
  (pkgId, importDir, exposedMods) <- queryPackage pkgName
  cache <- newIORef Map.empty
  builtins <- readGhcBuiltins
  builtinCache <- newIORef (Map.singleton "GHC.Prim" builtins)
  directories <- installedModuleDirectories pkgId
  let extraction = Extraction cache builtinCache directories
  modules <- mapM (extractInstalledModule extraction builtins importDir) exposedMods
  pure (PackageInterface (T.pack pkgId) modules)

extractSourcePackage :: FilePath -> String -> IO PackageInterface
extractSourcePackage root pkgName = do
  exposedMods <- exposedSourceModules root
  modules <- mapM (extractSourceModule (root </> "src")) exposedMods
  pure
    PackageInterface
      { piPackage = T.pack pkgName,
        piModules = modules
      }

type InterfaceCache = IORef (Map String GhcInterface)

data Extraction = Extraction
  { installedInterfaces :: InterfaceCache,
    builtinInterfaces :: InterfaceCache,
    moduleDirectories :: Map String [FilePath]
  }

extractInstalledModule :: Extraction -> GhcInterface -> FilePath -> String -> IO ModuleInterface
extractInstalledModule extraction builtins importDir modName = do
  exists <- doesFileExist (importDir </> map dotToSlash modName <.> "hi")
  iface <- if exists then loadInterface extraction importDir modName (const True) else pure (GhcInterface [] Map.empty Map.empty)
  if modName == "GHC.Prim" && not exists
    then do
      declarations <- mapM parseInstalledDeclaration ([raw | (name, _) <- ghcExports builtins, Just raw <- [Map.lookup (interfaceName name) (ghcDeclarations builtins)]])
      pure (mergeInterfaces (T.pack modName) declarations (Map.elems (ghcFixities builtins)))
    else do
      declarations <- mapM extractExport (ghcExports iface)
      fixities <- concat <$> mapM (exportedFixities iface) (ghcExports iface)
      pure (mergeInterfaces (T.pack modName) declarations fixities)
  where
    definingInterface contains reference = do
      let (owner, name) = splitReference modName reference
      ownerIface <- loadInterface extraction importDir owner (contains name)
      pure (owner, name, ownerIface)
    extractExport (reference, members) = do
      let parent = if null members then reference else fromMaybe reference (T.stripSuffix "|" reference)
      (owner, name, ownerIface) <- definingInterface (\name -> Map.member (interfaceName name) . ghcDeclarations) parent
      let localDeclaration = Map.lookup (interfaceName name) (ghcDeclarations ownerIface)
      declaration <- case localDeclaration of
        Just raw | null members || not ("pattern " `T.isPrefixOf` raw) -> pure (Just raw)
        _ -> do
          known <- readIORef (builtinInterfaces extraction)
          fallback <- case Map.lookup owner known of
            Just result -> pure result
            Nothing -> do
              result <- readGhcModule owner
              modifyIORef' (builtinInterfaces extraction) (Map.insert owner result)
              pure result
          pure (Map.lookup (interfaceName name) (ghcDeclarations fallback) <|> localDeclaration)
      case declaration of
        Nothing -> ioError (userError ("could not resolve export " <> T.unpack reference))
        Just raw -> do
          initial <- parseInstalledDeclaration raw
          let parsed = if Map.member (interfaceName name) (ghcDeclarations ownerIface) then initial else builtinClassType raw initial
          -- A separate constructor export does not add its parent type.
          let classes = filter ((== interfaceName name) . ecName) (miClasses parsed)
              types = if null classes then filter ((== interfaceName name) . etName) (miTypes parsed) else miTypes parsed
              addMembers typ = typ {etConstructors = nub (etConstructors typ <> filter (/= name) (map (unqualify . snd . splitReference modName) members))}
          let isData line = ("data " `T.isPrefixOf` line && not ("data family " `T.isPrefixOf` line)) || "newtype " `T.isPrefixOf` line
              exportedTypes = if null classes && any isData (T.lines raw) then map addMembers types else types
          pure parsed {miTypes = exportedTypes, miClasses = classes}
    exportedFixities localIface (reference, members) =
      concat <$> mapM (findFixity localIface) (if not (null members) && "|" `T.isSuffixOf` reference then members else reference : members)
    findFixity localIface reference =
      case Map.lookup (snd (splitReference modName reference)) (ghcFixities localIface) of
        Just fixity -> pure [fixity]
        Nothing -> do
          (_, name, ownerIface) <- definingInterface (\name -> Map.member name . ghcFixities) reference
          pure (maybe [] pure (Map.lookup name (ghcFixities ownerIface)))

loadInterface :: Extraction -> FilePath -> String -> (GhcInterface -> Bool) -> IO GhcInterface
loadInterface extraction importDir modName contains = do
  let directories = nub (importDir : Map.findWithDefault [] modName (moduleDirectories extraction))
  findInterface Nothing [dir </> map dotToSlash modName <.> "hi" | dir <- directories]
  where
    -- A package can re-export a name from another package with the same
    -- module name. Select the interface that declares the requested name.
    findInterface first [] = pure (fromMaybe (GhcInterface [] Map.empty Map.empty) first)
    findInterface first (path : rest) = do
      exists <- doesFileExist path
      if not exists
        then findInterface first rest
        else do
          known <- readIORef (installedInterfaces extraction)
          iface <- case Map.lookup path known of
            Just result -> pure result
            Nothing -> do
              result <- readGhcInterface path
              modifyIORef' (installedInterfaces extraction) (Map.insert path result)
              pure result
          if contains iface then pure iface else findInterface (first <|> Just iface) rest

-- | Include hidden modules. Exported names can refer to these modules.
installedModuleDirectories :: String -> IO (Map String [FilePath])
installedModuleDirectories packageId = do
  global <- readGhcPkg ["dump", "--expand-pkgroot"]
  localDb <- findLocalPackageDb
  local <- case localDb of
    Nothing -> pure ""
    Just db -> readProcess "ghc-pkg" ["--package-db", db, "dump", "--expand-pkgroot"] ""
  let packages = Map.fromList [(Map.findWithDefault "" "id" fields, fields) | package <- T.splitOn "\n---\n" (T.pack (global <> "\n---\n" <> local)), let fields = Map.fromList (packageFields (T.lines package))]
      dependencies seen [] = seen
      dependencies seen (unit : rest)
        | unit `elem` seen = dependencies seen rest
        | otherwise =
            let fields = Map.findWithDefault Map.empty unit packages
                required = map stripComma (words (Map.findWithDefault "" "depends" fields))
             in dependencies (unit : seen) (required <> rest)
      selected = [fields | unit <- dependencies [] [packageId], Just fields <- [Map.lookup unit packages]]
  pure (Map.fromListWith (<>) (concatMap packageDirectories selected))
  where
    packageDirectories fields =
      let modules = map stripComma (words (Map.findWithDefault "" "exposed-modules" fields <> " " <> Map.findWithDefault "" "hidden-modules" fields))
          directories = fieldPaths (Map.findWithDefault "" "import-dirs" fields)
       in [(name, directories) | name <- modules]
    packageFields [] = []
    packageFields (line : rest) =
      let (continuation, remaining) = span (T.isPrefixOf " ") rest
          (name, value) = T.breakOn ":" line
          field = T.unwords (T.drop 1 value : map T.strip continuation)
       in (T.unpack name, T.unpack (T.strip field)) : packageFields remaining
    fieldPaths input = case dropWhile (== ' ') input of
      [] -> []
      quoted@('"' : _) -> case reads quoted of
        [(path, rest)] -> path : fieldPaths rest
        _ -> []
      text -> let (path, rest) = break (== ' ') text in path : fieldPaths rest

splitReference :: String -> Text -> (String, Text)
splitReference fallback reference =
  let name = unqualify reference
      prefix = T.dropEnd (T.length name + 1) reference
   in (if T.null prefix then fallback else T.unpack prefix, name)

interfaceName :: Text -> Text
interfaceName name
  | name == "[]" = "List"
  | name == "()" = "Unit"
  | name == "(##)" = "Unit#"
  | Just body <- T.stripPrefix "(#" name >>= T.stripSuffix "#)",
    T.all (\c -> c == ',' || c == ' ' || c == '|') body =
      if T.any (== '|') body
        then "Sum" <> T.pack (show (T.count "|" body + 1)) <> "#"
        else "Tuple" <> T.pack (show (T.count "," body + 1)) <> "#"
  | Just body <- T.stripPrefix "(" name >>= T.stripSuffix ")", not (T.null body), T.all (== ',') body = "Tuple" <> T.pack (show (T.length body + 1))
  | otherwise = name

mergeInterfaces :: Text -> [ModuleInterface] -> [FixityInfo] -> ModuleInterface
mergeInterfaces name interfaces =
  ModuleInterface name (concatMap miTypes interfaces) (concatMap miValues interfaces) (concatMap miClasses interfaces)

parseInstalledDeclaration :: Text -> IO ModuleInterface
parseInstalledDeclaration raw =
  case T.lines raw of
    [] -> invalid
    first : rest
      | Just signature <- T.stripPrefix "type " first,
        (rawName, kind) <- T.breakOn " :: " signature,
        let name = stripParens rawName,
        not (T.null kind) -> do
          let kindText = T.drop 4 kind
              declaration = T.unlines rest
              associated = [ExportedType n (T.drop 4 k) [] | line <- rest, Just body <- [T.stripPrefix "  type " line], let (n, k) = T.breakOn " :: " body, not (T.null k)]
          if any (T.isPrefixOf "class ") rest
            then pure (empty {miTypes = associated, miClasses = [ExportedClass name (mapMaybe classMethod rest)]})
            else do
              constructors <-
                if any (\line -> "data " `T.isPrefixOf` line || "newtype " `T.isPrefixOf` line) rest
                  then installedConstructors name declaration
                  else pure []
              pure (empty {miTypes = [ExportedType name kindText constructors]})
      | (name, signature) <- T.breakOn " :: " raw,
        not (T.null signature) ->
          pure (empty {miValues = [ExportedValue (stripParens (fromMaybe name (T.stripPrefix "pattern " name))) (T.strip (T.drop 4 signature))]})
      | otherwise -> invalid
  where
    empty = ModuleInterface "" [] [] [] []
    invalid = ioError (userError ("could not parse GHC declaration: " <> T.unpack raw))
    classMethod line = do
      body <- T.stripPrefix "  " line
      let (name, signature) = T.breakOn " :: " body
      if T.null signature || "type " `T.isPrefixOf` name || "default " `T.isPrefixOf` name
        then Nothing
        else Just (ClassMethod (stripParens name) (T.drop 4 signature))

-- | The previous extractor reports built-in constraint classes as types.
-- Their dictionary constructors are compiler names, not source exports.
builtinClassType :: Text -> ModuleInterface -> ModuleInterface
builtinClassType raw iface = case miClasses iface of
  [klass] ->
    let name = ecName klass
        kind = T.drop 4 (snd (T.breakOn " :: " (T.takeWhile (/= '\n') raw)))
        constructor = case name of
          "Coercible" -> "MkCoercible"
          "~" -> "Eq#"
          "~~" -> "HEq#"
          _ -> name
     in iface {miTypes = [ExportedType name kind [constructor]], miClasses = []}
  _ -> iface

stripParens :: Text -> Text
stripParens text = fromMaybe text (T.stripPrefix "(" text >>= T.stripSuffix ")")

installedConstructors :: Text -> Text -> IO [Text]
installedConstructors name declaration
  | " = (," `T.isInfixOf` declaration, Just arity <- T.stripPrefix "Tuple" name >>= readArity = pure ["(" <> T.replicate (arity - 1) "," <> ")"]
  | name == "Unit", " = ()" `T.isInfixOf` declaration = pure ["()"]
  | name == "List", " = []" `T.isInfixOf` declaration = pure ["[]", ":"]
  | " = (#" `T.isInfixOf` declaration, Just arity <- T.stripPrefix "Tuple" name >>= T.stripSuffix "#" >>= readArity = pure ["(#" <> T.replicate (arity - 1) "," <> "#)"]
  | name == "Unit#", " = (##)" `T.isInfixOf` declaration = pure ["(##)"]
  | " = (#" `T.isInfixOf` declaration, Just _ <- T.stripPrefix "Sum" name >>= T.stripSuffix "#" >>= readArity = pure (sumConstructors declaration)
  | otherwise = do
      let (errors, modu) = parseModule defaultConfig ("{-# LANGUAGE BangPatterns, MagicHash, UnboxedTuples, UnboxedSums, GADTs, DataKinds, ExplicitForAll, ExistentialQuantification, KindSignatures, TypeOperators, UnliftedNewtypes, LinearTypes #-}\nmodule Interface where\n" <> T.replace " !" " " (removeUnpackCoercions declaration))
      if null errors
        then pure (concatMap (declarationConstructors . peelDeclAnn) (moduleDecls modu))
        else ioError (userError ("could not parse GHC data declaration: " <> T.unpack declaration <> ": " <> show errors))
  where
    declarationConstructors decl = case decl of
      DeclData dataDecl -> concatMap constructorNames (dataDeclConstructors dataDecl)
      DeclNewtype newtypeDecl -> maybe [] constructorNames (newtypeDeclConstructor newtypeDecl)
      _ -> []
    constructorNames con = case peelDataConAnn con of
      RecordCon _ _ constructor _ -> [renderUnqualifiedName constructor]
      GadtCon _ _ constructors _ -> map renderUnqualifiedName constructors
      other -> dataConNames other
    sumConstructors text = case T.breakOn "(#" text of
      (_, suffix) | T.null suffix -> []
      (_, suffix) -> let (constructor, rest) = T.breakOn "#)" suffix in (constructor <> "#)") : sumConstructors (T.drop 2 rest)
    readArity :: Text -> Maybe Int
    readArity text = case reads (T.unpack text) of
      [(arity, "")] -> Just arity
      _ -> Nothing

-- | Interface fields can contain an unpack coercion before the source type.
removeUnpackCoercions :: Text -> Text
removeUnpackCoercions text = case T.breakOn "{-# UNPACK #-}" text of
  (_, rest) | T.null rest -> text
  (before, rest) ->
    let after = T.stripStart (T.drop 14 rest)
        (group, suffix) = balancedGroup after
        field = if ":" `T.isInfixOf` group then suffix else after
     in before <> removeUnpackCoercions field
  where
    balancedGroup input = case T.uncons input of
      Just ('(', rest) -> go 1 "(" rest
      _ -> ("", input)
    go :: Int -> Text -> Text -> (Text, Text)
    go depth prefix input = case T.uncons input of
      Nothing -> (prefix, "")
      Just (char, rest) ->
        let next = depth + if char == '(' then 1 else if char == ')' then -1 else 0
            result = T.snoc prefix char
         in if next == 0 then (result, rest) else go next result rest

exposedSourceModules :: FilePath -> IO [String]
exposedSourceModules root = do
  cabalFiles <- filter ((".cabal" ==) . takeExtension) <$> listDirectory root
  case cabalFiles of
    cabalFile : _ -> do
      contents <- BS.readFile (root </> cabalFile)
      case parsePackageDescription contents of
        Right gpd -> pure (genericPackageExposedModules gpd)
        Left errors -> ioError (userError ("could not parse " <> cabalFile <> ": " <> errors))
    [] -> ioError (userError ("no Cabal file found under " <> root))

genericPackageExposedModules :: Package -> [String]
genericPackageExposedModules gpd =
  [ moduleNameFilePath modName
  | Component (Library MainLibrary) libTree <- packageComponents gpd,
    modName <- exposedModules (unconditional libTree)
  ]

extractSourceModule :: FilePath -> String -> IO ModuleInterface
extractSourceModule srcRoot modPath = do
  let sourcePath = srcRoot </> modPath <.> "hs"
      modName = T.pack (map pathSepToDot modPath)
  exists <- doesFileExist sourcePath
  if not exists
    then ioError (userError ("source module " <> T.unpack modName <> " not found at " <> sourcePath))
    else do
      source <- TE.decodeUtf8 <$> BS.readFile sourcePath
      let (errs, parsed) =
            parseModule
              (defaultConfig {parserSourceName = sourcePath})
              source
      if null errs
        then sourceModuleInterface modName parsed
        else ioError (userError ("source module " <> T.unpack modName <> " parse failed: " <> show errs))

sourceModuleInterface :: Text -> Module -> IO ModuleInterface
sourceModuleInterface fallbackName modu =
  let iface =
        applySourceExports modu $
          ModuleInterface
            { miModule = fromMaybe fallbackName (Syntax.moduleName modu),
              miTypes = sourceTypes kindSigs decls,
              miValues = sourceValues decls,
              miClasses = sourceClasses decls,
              miFixities = sourceFixities decls
            }
   in case missingExportedValueSignatures modu decls iface of
        [] -> pure iface
        missing ->
          ioError
            ( userError
                ( "source module "
                    <> T.unpack (miModule iface)
                    <> " has exported value(s) without type signatures: "
                    <> T.unpack (T.intercalate ", " missing)
                )
            )
  where
    decls = map peelDeclAnn (moduleDecls modu)
    kindSigs = Map.fromList [(renderUnqualifiedName name, renderSourceType kind) | DeclStandaloneKindSig name kind <- decls]

sourceValues :: [Decl] -> [ExportedValue]
sourceValues decls =
  Map.elems $
    Map.fromList
      [ (name, ExportedValue name (renderSourceType ty))
      | (name, ty) <- concatMap declaredValueTypes decls
      ]
  where
    declaredValueTypes decl =
      case decl of
        DeclTypeSig names ty -> [(renderUnqualifiedName name, ty) | name <- names]
        DeclForeign foreignDecl
          | foreignDirection foreignDecl == ForeignImport ->
              [(renderUnqualifiedName (foreignName foreignDecl), foreignType foreignDecl)]
        _ -> []

missingExportedValueSignatures :: Module -> [Decl] -> ModuleInterface -> [Text]
missingExportedValueSignatures modu decls iface =
  [ name
  | name <- candidateNames,
    Map.notMember name typedValues
  ]
  where
    typedValues = Map.fromList [(evName value, ()) | value <- miValues iface]
    declaredValues =
      Map.fromList [(name, ()) | DeclValue valueDecl <- decls, name <- valueDeclNames valueDecl]
    candidateNames =
      case moduleExports modu of
        Nothing -> Map.keys declaredValues
        Just specs ->
          [ name
          | name <- concatMap exportValueNames specs,
            Map.member name declaredValues
          ]

valueDeclNames :: ValueDecl -> [Text]
valueDeclNames valueDecl =
  case valueDecl of
    FunctionBind name _ -> [renderUnqualifiedName name]
    PatternBind _ pat _ -> patternBinderNames pat

patternBinderNames :: Pattern -> [Text]
patternBinderNames pat =
  case pat of
    PAnn _ inner -> patternBinderNames inner
    PVar name -> [renderUnqualifiedName name]
    _ -> []

sourceTypes :: Map Text Text -> [Decl] -> [ExportedType]
sourceTypes kindSigs =
  mapMaybe go
  where
    go decl =
      case decl of
        DeclTypeSyn syn ->
          let name = binderHeadName (typeSynHead syn)
           in Just (ExportedType name (sourceTypeKind kindSigs name) [])
        DeclTypeData dataDecl -> Just (sourceDataType kindSigs dataDecl)
        DeclData dataDecl -> Just (sourceDataType kindSigs dataDecl)
        DeclNewtype newtypeDecl -> Just (sourceNewtype kindSigs newtypeDecl)
        DeclTypeFamilyDecl familyDecl ->
          let name = sourceTypeFamilyName familyDecl
           in Just (ExportedType name (sourceTypeKind kindSigs name) [])
        _ -> Nothing

sourceDataType :: Map Text Text -> DataDecl -> ExportedType
sourceDataType kindSigs dataDecl =
  ExportedType
    { etName = name,
      etKind = sourceTypeKind kindSigs name,
      etConstructors = concatMap dataConNames (dataDeclConstructors dataDecl)
    }
  where
    name = binderHeadName (dataDeclHead dataDecl)

sourceNewtype :: Map Text Text -> NewtypeDecl -> ExportedType
sourceNewtype kindSigs newtypeDecl =
  ExportedType
    { etName = name,
      etKind = sourceTypeKind kindSigs name,
      etConstructors = maybe [] dataConNames (newtypeDeclConstructor newtypeDecl)
    }
  where
    name = binderHeadName (newtypeDeclHead newtypeDecl)

-- | The full kind of a source type: only a standalone kind signature states
-- it. A kind annotation on a declaration head gives just the result kind,
-- which is not comparable with the kinds extracted from interface files.
sourceTypeKind :: Map Text Text -> Text -> Text
sourceTypeKind kindSigs name =
  Map.findWithDefault unspecifiedSourceKind name kindSigs

sourceClasses :: [Decl] -> [ExportedClass]
sourceClasses decls =
  [ ExportedClass
      { ecName = binderHeadName (classDeclHead classDecl),
        ecMethods = sourceClassMethods classDecl
      }
  | DeclClass classDecl <- decls
  ]

sourceClassMethods :: ClassDecl -> [ClassMethod]
sourceClassMethods classDecl =
  [ ClassMethod (renderUnqualifiedName name) (renderSourceType ty)
  | item <- map peelClassDeclItemAnn (classDeclItems classDecl),
    ClassItemTypeSig names ty <- [item],
    name <- names
  ]

sourceFixities :: [Decl] -> [FixityInfo]
sourceFixities decls =
  [ FixityInfo
      { fiName = renderUnqualifiedName op,
        fiDirection = sourceFixityDirection assoc,
        fiPrecedence = fromMaybe 9 mPrec
      }
  | DeclFixity assoc _ mPrec ops <- decls,
    op <- ops
  ]

sourceFixityDirection :: FixityAssoc -> FixityDirection
sourceFixityDirection assoc =
  case assoc of
    Syntax.InfixL -> Aihc.Dev.ExtractHi.Types.InfixL
    Syntax.InfixR -> Aihc.Dev.ExtractHi.Types.InfixR
    Syntax.Infix -> Aihc.Dev.ExtractHi.Types.InfixN

applySourceExports :: Module -> ModuleInterface -> ModuleInterface
applySourceExports modu iface =
  case moduleExports modu of
    Nothing -> iface
    Just specs ->
      iface
        { miValues = filter (exportedValue allowedValues) (miValues iface),
          miTypes = mapMaybe (filterExportedType allowedTypes allowedConstructors) (miTypes iface),
          miClasses = mapMaybe (filterExportedClass allowedTypes allowedMethods) (miClasses iface),
          miFixities = filter (exportedFixity allowedValues allowedTypes) (miFixities iface)
        }
      where
        allowedValues = Map.fromList [(name, ()) | name <- concatMap exportValueNames specs]
        allowedTypes = Map.fromList [(name, exportMembers spec) | spec <- specs, name <- exportTypeNames spec]
        allowedConstructors = Map.unionsWith (<>) [Map.singleton parent members | spec <- specs, (parent, members) <- exportTypeMemberNames spec]
        allowedMethods = Map.unionsWith (<>) [Map.singleton parent members | spec <- specs, (parent, members) <- exportTypeMemberNames spec]

exportedValue :: Map Text () -> ExportedValue -> Bool
exportedValue allowed value =
  Map.member (evName value) allowed

filterExportedType :: Map Text (Maybe [Text]) -> Map Text [Text] -> ExportedType -> Maybe ExportedType
filterExportedType allowedTypes allowedConstructors typ =
  case Map.lookup (etName typ) allowedTypes of
    Nothing -> Nothing
    Just Nothing -> Just (typ {etConstructors = []})
    Just (Just []) -> Just typ
    Just (Just names) -> Just (typ {etConstructors = filter (`elem` names) (etConstructors typ)})
  where
    _ = allowedConstructors

filterExportedClass :: Map Text (Maybe [Text]) -> Map Text [Text] -> ExportedClass -> Maybe ExportedClass
filterExportedClass allowedTypes allowedMethods klass =
  case Map.lookup (ecName klass) allowedTypes of
    Nothing -> Nothing
    Just Nothing -> Just (klass {ecMethods = []})
    Just (Just []) -> Just klass
    Just (Just names) -> Just (klass {ecMethods = filter ((`elem` names) . cmName) (ecMethods klass)})
  where
    _ = allowedMethods

exportedFixity :: Map Text () -> Map Text (Maybe [Text]) -> FixityInfo -> Bool
exportedFixity allowedValues allowedTypes fixity =
  Map.member (fiName fixity) allowedValues || Map.member (fiName fixity) allowedTypes

exportValueNames :: ExportSpec -> [Text]
exportValueNames spec =
  case spec of
    ExportAnn _ inner -> exportValueNames inner
    ExportVar _ namespace name | namespace /= Just IEEntityNamespaceType -> [renderName name]
    _ -> []

exportTypeNames :: ExportSpec -> [Text]
exportTypeNames spec =
  case spec of
    ExportAnn _ inner -> exportTypeNames inner
    ExportVar _ (Just IEEntityNamespaceType) name -> [renderName name]
    ExportAbs _ _ name -> [renderName name]
    ExportAll _ _ name -> [renderName name]
    ExportWith _ _ name _ -> [renderName name]
    ExportWithAll _ _ name _ _ -> [renderName name]
    _ -> []

exportTypeMemberNames :: ExportSpec -> [(Text, [Text])]
exportTypeMemberNames spec =
  case spec of
    ExportAnn _ inner -> exportTypeMemberNames inner
    ExportWith _ _ name members -> [(renderName name, map (renderName . ieBundledMemberName) members)]
    ExportWithAll _ _ name _ members -> [(renderName name, map (renderName . ieBundledMemberName) members)]
    _ -> []

exportMembers :: ExportSpec -> Maybe [Text]
exportMembers spec =
  case spec of
    ExportAnn _ inner -> exportMembers inner
    ExportAbs {} -> Nothing
    ExportAll {} -> Just []
    ExportWith _ _ _ members -> Just (map (renderName . ieBundledMemberName) members)
    ExportWithAll _ _ _ _ members -> Just (map (renderName . ieBundledMemberName) members)
    _ -> Nothing

binderHeadName :: BinderHead UnqualifiedName -> Text
binderHeadName head' =
  case head' of
    PrefixBinderHead name _ -> renderUnqualifiedName name
    InfixBinderHead _ name _ _ -> renderUnqualifiedName name

sourceTypeFamilyName :: TypeFamilyDecl -> Text
sourceTypeFamilyName familyDecl =
  case typeFamilyDeclHead familyDecl of
    TCon name _ -> renderName name
    _ -> renderSourceType (typeFamilyDeclHead familyDecl)

dataConNames :: DataConDecl -> [Text]
dataConNames con =
  case peelDataConAnn con of
    PrefixCon _ _ name _ -> [renderUnqualifiedName name]
    InfixCon _ _ _ name _ -> [renderUnqualifiedName name]
    RecordCon _ _ name fields -> renderUnqualifiedName name : concatMap fieldDeclNames fields
    GadtCon _ _ names body -> map renderUnqualifiedName names <> gadtBodyFieldNames body
    TupleCon {} -> []
    UnboxedSumCon {} -> []
    ListCon {} -> ["[]"]
    DataConAnn {} -> []

gadtBodyFieldNames :: GadtBody -> [Text]
gadtBodyFieldNames body =
  case body of
    GadtPrefixBody {} -> []
    GadtRecordBody fields _ -> concatMap fieldDeclNames fields

fieldDeclNames :: FieldDecl -> [Text]
fieldDeclNames field =
  map renderUnqualifiedName (fieldNames field)

renderSourceType :: Type -> Text
renderSourceType =
  T.pack . renderString . layoutPretty defaultLayoutOptions . prettyType

-- | Query @ghc-pkg@ for package information.
--
-- Returns (package-id, import-dir, [exposed-module-names]).
queryPackage :: String -> IO (String, FilePath, [String])
queryPackage pkgName = do
  pkgId <- trim <$> readGhcPkg ["field", pkgName, "id", "--simple-output"]
  importDir <- trim <$> readGhcPkg ["field", pkgName, "import-dirs", "--simple-output"]
  exposedModsRaw <- readGhcPkg ["field", pkgName, "exposed-modules", "--simple-output"]
  let exposedMods = map stripComma (words exposedModsRaw)
  pure (pkgId, importDir, exposedMods)

readGhcPkg :: [String] -> IO String
readGhcPkg args = do
  result <- tryReadGhcPkgRaw args
  case result of
    Just output -> pure output
    Nothing -> do
      mPackageDb <- findLocalPackageDb
      case mPackageDb of
        Nothing -> missingPackage
        Just packageDb -> do
          fallbackResult <- tryReadGhcPkgRaw ("--package-db" : packageDb : args)
          maybe missingPackage pure fallbackResult
  where
    missingPackage = ioError (userError ("ghc-pkg failed: " <> unwords args))

tryReadGhcPkgRaw :: [String] -> IO (Maybe String)
tryReadGhcPkgRaw args = do
  (code, output, _) <- readProcessWithExitCode "ghc-pkg" args ""
  pure $
    case code of
      ExitSuccess -> Just output
      ExitFailure _ -> Nothing

findLocalPackageDb :: IO (Maybe FilePath)
findLocalPackageDb = do
  cwd <- getCurrentDirectory
  ghcVersion <- trim <$> readProcess "ghc" ["--numeric-version"] ""
  findAncestorContaining ("dist-newstyle" </> "packagedb" </> ("ghc-" <> ghcVersion)) cwd
  where
    findAncestorContaining rel dir = do
      let packageDb = dir </> rel
      exists <- doesDirectoryExist packageDb
      if exists
        then pure (Just packageDb)
        else do
          let parent = takeDirectory dir
          if parent == dir
            then pure Nothing
            else findAncestorContaining rel parent

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

pathSepToDot :: Char -> Char
pathSepToDot '/' = '.'
pathSepToDot c = c
