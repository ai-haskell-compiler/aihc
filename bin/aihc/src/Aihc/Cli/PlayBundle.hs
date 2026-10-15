{-# LANGUAGE OverloadedStrings #-}

-- | The bundle of naming facts and type facts that the playground helper
-- of The Lazy Reader loads instead of the core libraries.
--
-- The guide compiles its helper to WebAssembly, and a browser cannot
-- compile the core libraries in a time a reader waits for. The bundle
-- holds what the front end needs of them: the exports of every module,
-- for name resolution, and the type facts, for the type checker.
--
-- A bundle is one file in two parts:
--
-- * A header, decoded at once. It names the primitive package, holds the
--   resolve artifact of every module, and holds the index of the type
--   section: the key of every fact with the offset and the length of
--   its bytes.
--
-- * The type section, decoded one fact at a time. Each fact is one type
--   interface with one entry, in the format of "Aihc.Cli.TypeArtifact".
--   A fact self-contained, so the helper decodes only the facts the type
--   checker looks up.
--
-- The loaded interface maps hold a thunk for each fact. The type checker
-- forces a thunk when it looks the fact up, and the decoder runs then.
--
-- The store formats do not change. The bundle holds resolve artifacts as
-- the store writes them, and it encodes each type fact with the encoder
-- of the type artifacts.
module Aihc.Cli.PlayBundle
  ( BundleModule (..),
    encodeBundle,
    LoadedBundle (..),
    BundleStats (..),
    loadBundle,
  )
where

import Aihc.Cbor (cborArray, cborBytes, cborInt, cborText, cborWord, getArrayLength, getInt, getMajor, getText, getWord, (<*!>))
import Aihc.Cli.ResolveArtifact (ResolveArtifact (..), decodeResolveArtifact)
import Aihc.Cli.TypeArtifact (decodeTypeInterface, encodeTypeInterface, getTermKey, putTermKey)
import Aihc.Resolve (Entity (..), GlobalName, ModuleExports, ModuleKey (..), Package (..), PackageId (..), moduleExportsFromList)
import Aihc.Tc
  ( InstanceKey,
    MergeCheck (..),
    TcAxiomKey (..),
    TcInterface (..),
    emptyTcInterface,
    mergeTcInterfaces,
  )
import Control.Monad (replicateM, unless, (<$!>))
import Data.Binary.Get qualified as Get
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as BL
import Data.Map.Lazy qualified as LazyMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T

-- | One module of the bundle: its package and its resolve artifact, as
-- the store holds it.
data BundleModule = BundleModule
  { bundleModulePackage :: !Package,
    bundleModuleName :: !Text,
    bundleModuleResolve :: !BS.ByteString
  }

-- | What a loaded bundle gives the helper.
data LoadedBundle = LoadedBundle
  { -- | The identity of the primitive package, for the type checker
    -- configuration.
    loadedPrim :: !PackageId,
    loadedExports :: !ModuleExports,
    -- | The type facts. Each map value is a thunk over the bytes of the
    -- fact.
    loadedInterface :: !TcInterface,
    loadedStats :: !BundleStats
  }

data BundleStats = BundleStats
  { statsModules :: !Int,
    statsNamingBytes :: !Int,
    statsTypeBytes :: !Int,
    statsFacts :: !Int
  }
  deriving (Show)

bundleMagic :: Text
bundleMagic = "aihc-play-bundle-1"

-- | The index of the type section: the key of each fact with the offset
-- and the length of its bytes, one list per kind of fact.
data Index = Index
  { indexTerms :: [(Key, Int, Int)],
    indexTyCons :: [(Key, Int, Int)],
    indexDataTypes :: [(Key, Int, Int)],
    indexClasses :: [(Key, Int, Int)],
    indexInstances :: [(Key, Int, Int)],
    indexDataFamilyInstances :: [(Key, Int, Int)],
    indexTypeFamilyInstances :: [(Key, Int, Int)],
    indexPatSyns :: [(Key, Int, Int)],
    indexForeignImports :: [(Key, Int, Int)]
  }

-- | A key of the index. Each kind of fact has its own key type in the
-- interface; the index writes them in one form.
data Key
  = KeyEntity !Entity
  | KeyGlobal !GlobalName
  | KeyInstance !InstanceKey
  | KeyAxiom !TcAxiomKey

-- | Write a bundle: the primitive package identity, the modules, and the
-- type interfaces of the modules.
encodeBundle :: PackageId -> [BundleModule] -> [TcInterface] -> BL.ByteString
encodeBundle prim modules interfaces =
  Builder.toLazyByteString (putHeader <> Builder.lazyByteString typeSection)
  where
    interface = mergeTcInterfaces TrustMergedFacts interfaces
    (index, typeSection) = encodeTypeSection interface
    putHeader =
      cborArray 6
        <> cborText bundleMagic
        <> putPackageId prim
        <> cborInt (tcInterfaceUniqueBound interface)
        <> encodeList putModule modules
        <> putIndex index
        <> cborInt (fromIntegral (BL.length typeSection))
    putModule modu =
      cborArray 4
        <> cborText (packageName (bundleModulePackage modu))
        <> putPackageId (packageId (bundleModulePackage modu))
        <> cborText (bundleModuleName modu)
        <> cborBytes (bundleModuleResolve modu)

-- | The type section and its index. Each fact becomes an interface with
-- that fact alone, so the artifact encoder writes it with the type
-- constructors and the parts it needs and nothing else.
encodeTypeSection :: TcInterface -> (Index, BL.ByteString)
encodeTypeSection interface =
  let (terms, afterTerms) = facts 0 KeyEntity (\k v -> emptyTcInterface {tcInterfaceTermMap = Map.singleton k v}) (tcInterfaceTermMap interface)
      (tyCons, afterTyCons) = facts afterTerms KeyGlobal (\k v -> emptyTcInterface {tcInterfaceTyConMap = Map.singleton k v}) (tcInterfaceTyConMap interface)
      (dataTypes, afterDataTypes) = facts afterTyCons KeyGlobal (\k v -> emptyTcInterface {tcInterfaceDataTypeMap = Map.singleton k v}) (tcInterfaceDataTypeMap interface)
      (classes, afterClasses) = facts afterDataTypes KeyGlobal (\k v -> emptyTcInterface {tcInterfaceClassMap = Map.singleton k v}) (tcInterfaceClassMap interface)
      (instances, afterInstances) = facts afterClasses KeyInstance (\k v -> emptyTcInterface {tcInterfaceInstanceMap = Map.singleton k v}) (tcInterfaceInstanceMap interface)
      (dataFamilies, afterDataFamilies) = facts afterInstances KeyAxiom (\k v -> emptyTcInterface {tcInterfaceDataFamilyInstanceMap = Map.singleton k v}) (tcInterfaceDataFamilyInstanceMap interface)
      (typeFamilies, afterTypeFamilies) = facts afterDataFamilies KeyAxiom (\k v -> emptyTcInterface {tcInterfaceTypeFamilyInstanceMap = Map.singleton k v}) (tcInterfaceTypeFamilyInstanceMap interface)
      (patSyns, afterPatSyns) = facts afterTypeFamilies KeyEntity (\k v -> emptyTcInterface {tcInterfacePatSynMap = Map.singleton k v}) (tcInterfacePatSynMap interface)
      (foreignImports, afterForeignImports) = facts afterPatSyns KeyEntity (\k v -> emptyTcInterface {tcInterfaceForeignImportMap = Map.singleton k v}) (tcInterfaceForeignImportMap interface)
      index =
        Index
          { indexTerms = fst terms,
            indexTyCons = fst tyCons,
            indexDataTypes = fst dataTypes,
            indexClasses = fst classes,
            indexInstances = fst instances,
            indexDataFamilyInstances = fst dataFamilies,
            indexTypeFamilyInstances = fst typeFamilies,
            indexPatSyns = fst patSyns,
            indexForeignImports = fst foreignImports
          }
      section = mconcat (map snd [terms, tyCons, dataTypes, classes, instances, dataFamilies, typeFamilies, patSyns, foreignImports])
   in afterForeignImports `seq` (index, Builder.toLazyByteString section)
  where
    -- The entries of one map, each as its own interface, placed one
    -- after the other from the given offset.
    facts :: Int -> (key -> Key) -> (key -> value -> TcInterface) -> Map key value -> (([(Key, Int, Int)], Builder.Builder), Int)
    facts start toKey singleton values =
      let step (entries, builder, offset) (key, value) =
            let bytes = encodeTypeInterface (singleton key value)
                size = fromIntegral (BL.length bytes)
             in ((toKey key, offset, size) : entries, builder <> Builder.lazyByteString bytes, offset + size)
          (entries', builder', end) = foldl step ([], mempty, start) (Map.toAscList values)
       in ((reverse entries', builder'), end)

putIndex :: Index -> Builder.Builder
putIndex index =
  cborArray 9
    <> encodeList putEntry (indexTerms index)
    <> encodeList putEntry (indexTyCons index)
    <> encodeList putEntry (indexDataTypes index)
    <> encodeList putEntry (indexClasses index)
    <> encodeList putEntry (indexInstances index)
    <> encodeList putEntry (indexDataFamilyInstances index)
    <> encodeList putEntry (indexTypeFamilyInstances index)
    <> encodeList putEntry (indexPatSyns index)
    <> encodeList putEntry (indexForeignImports index)
  where
    putEntry (key, offset, size) = cborArray 3 <> putKey key <> cborInt offset <> cborInt size

putKey :: Key -> Builder.Builder
putKey key =
  case key of
    KeyEntity entity -> putTermKey entity
    KeyGlobal name -> putTermKey (EntityGlobal name)
    KeyInstance ((package, moduleName), dictName) ->
      cborArray 4 <> cborWord 3 <> cborText package <> cborText moduleName <> cborText dictName
    KeyAxiom (TcAxiomKey package moduleName name) ->
      cborArray 4 <> cborWord 4 <> putPackageId package <> cborText moduleName <> cborText name

-- | Decode a bundle. The header and the index are decoded now; the type
-- section is kept as bytes, and each fact is decoded when the type
-- checker looks it up.
loadBundle :: BS.ByteString -> Either String LoadedBundle
loadBundle bytes =
  case Get.runGetOrFail getHeader (BL.fromStrict bytes) of
    Left (_, _, message) -> Left ("invalid bundle: " <> message)
    Right (_, consumed, header) -> do
      let section = BS.drop (fromIntegral consumed) bytes
      unless (BS.length section == headerTypeBytes header) $
        Left "invalid bundle: the type section has the wrong length"
      exports <- traverse moduleExports (headerModules header)
      let index = headerIndex header
          interface =
            TcInterface
              { tcInterfaceTermMap = factMap section keyEntity tcInterfaceTermMap (indexTerms index),
                tcInterfaceTyConMap = factMap section keyGlobal tcInterfaceTyConMap (indexTyCons index),
                tcInterfaceDataTypeMap = factMap section keyGlobal tcInterfaceDataTypeMap (indexDataTypes index),
                tcInterfaceClassMap = factMap section keyGlobal tcInterfaceClassMap (indexClasses index),
                tcInterfaceInstanceMap = factMap section keyInstance tcInterfaceInstanceMap (indexInstances index),
                tcInterfaceDataFamilyInstanceMap = factMap section keyAxiom tcInterfaceDataFamilyInstanceMap (indexDataFamilyInstances index),
                tcInterfaceTypeFamilyInstanceMap = factMap section keyAxiom tcInterfaceTypeFamilyInstanceMap (indexTypeFamilyInstances index),
                tcInterfacePatSynMap = factMap section keyEntity tcInterfacePatSynMap (indexPatSyns index),
                tcInterfaceForeignImportMap = factMap section keyEntity tcInterfaceForeignImportMap (indexForeignImports index),
                tcInterfaceUniqueBound = headerUniqueBound header
              }
          facts =
            sum
              ( map
                  length
                  [ indexTerms index,
                    indexTyCons index,
                    indexDataTypes index,
                    indexClasses index,
                    indexInstances index,
                    indexDataFamilyInstances index,
                    indexTypeFamilyInstances index,
                    indexPatSyns index,
                    indexForeignImports index
                  ]
              )
      pure
        LoadedBundle
          { loadedPrim = headerPrim header,
            loadedExports = moduleExportsFromList exports,
            loadedInterface = interface,
            loadedStats =
              BundleStats
                { statsModules = length (headerModules header),
                  statsNamingBytes = sum [BS.length (bundleModuleResolve modu) | modu <- headerModules header],
                  statsTypeBytes = BS.length section,
                  statsFacts = facts
                }
          }
  where
    moduleExports modu = do
      artifact <- either (\message -> Left ("invalid resolve artifact of " <> T.unpack (bundleModuleName modu) <> ": " <> message)) Right (decodeResolveArtifact (bundleModuleResolve modu))
      pure (ModuleKey (bundleModulePackage modu) (bundleModuleName modu), resolveArtifactExports artifact)

-- | A map whose values decode their fact on first use. The map itself is
-- built now: the type checker looks keys up, and a lazy map still holds
-- every key. The writer lists the entries in key order, so the map is
-- built in one pass.
factMap :: (Ord key) => BS.ByteString -> (Key -> key) -> (TcInterface -> Map key value) -> [(Key, Int, Int)] -> Map key value
factMap section project select entries =
  LazyMap.fromDistinctAscList
    [ (project key, fact offset size)
    | (key, offset, size) <- entries
    ]
  where
    fact offset size =
      case decodeTypeInterface (BL.fromStrict (BS.take size (BS.drop offset section))) of
        Left message -> error ("corrupt bundle fact at offset " <> show offset <> ": " <> message)
        Right interface ->
          case Map.elems (select interface) of
            [value] -> value
            _ -> error ("corrupt bundle fact at offset " <> show offset <> ": not one fact")

keyEntity :: Key -> Entity
keyEntity key = case key of
  KeyEntity entity -> entity
  KeyGlobal name -> EntityGlobal name
  _ -> error "corrupt bundle index: not a term key"

keyGlobal :: Key -> GlobalName
keyGlobal key = case key of
  KeyGlobal name -> name
  _ -> error "corrupt bundle index: not a global name key"

keyInstance :: Key -> InstanceKey
keyInstance key = case key of
  KeyInstance instanceKey -> instanceKey
  _ -> error "corrupt bundle index: not an instance key"

keyAxiom :: Key -> TcAxiomKey
keyAxiom key = case key of
  KeyAxiom axiom -> axiom
  _ -> error "corrupt bundle index: not an axiom key"

data Header = Header
  { headerPrim :: !PackageId,
    headerUniqueBound :: !Int,
    headerModules :: ![BundleModule],
    headerIndex :: !Index,
    headerTypeBytes :: !Int
  }

getHeader :: Get.Get Header
getHeader = do
  expectArray 6
  magic <- getText
  unless (magic == bundleMagic) (fail ("not a bundle: " <> T.unpack magic))
  headerPrim <- getPackageId
  headerUniqueBound <- getInt
  headerModules <- getList getModule
  headerIndex <- getIndex
  headerTypeBytes <- getInt
  pure Header {headerPrim, headerUniqueBound, headerModules, headerIndex, headerTypeBytes}
  where
    getModule = do
      expectArray 4
      name <- getText
      identity <- getPackageId
      moduleName <- getText
      -- Not copied: the bytes are a slice of the bundle, which stays
      -- loaded for the type section anyway.
      BundleModule (Package name identity) moduleName <$> getSlice

getIndex :: Get.Get Index
getIndex = do
  expectArray 9
  Index
    <$!> getList getEntry
      <*!> getList getEntry
      <*!> getList getEntry
      <*!> getList getEntry
      <*!> getList getEntry
      <*!> getList getEntry
      <*!> getList getEntry
      <*!> getList getEntry
      <*!> getList getEntry
  where
    getEntry = do
      expectArray 3
      key <- getKey
      offset <- getInt
      size <- getInt
      pure (key, offset, size)

getKey :: Get.Get Key
getKey = do
  initial <- Get.lookAhead (getArrayLength >> getWord)
  case initial of
    3 -> do
      expectArray 4
      _ <- getWord
      package <- getText
      moduleName <- getText
      dictName <- getText
      pure (KeyInstance ((package, moduleName), dictName))
    4 -> do
      expectArray 4
      _ <- getWord
      package <- getPackageId
      moduleName <- getText
      KeyAxiom . TcAxiomKey package moduleName <$> getText
    _ -> do
      entity <- getTermKey
      pure $! case entity of
        EntityGlobal name -> KeyGlobal name
        _ -> KeyEntity entity

putPackageId :: PackageId -> Builder.Builder
putPackageId (PackageId identity) = cborText identity

getPackageId :: Get.Get PackageId
getPackageId = PackageId <$!> getText

-- | A CBOR byte string without a copy of the input.
getSlice :: Get.Get BS.ByteString
getSlice = do
  size <- getMajor 2
  Get.getByteString (fromIntegral size)

encodeList :: (value -> Builder.Builder) -> [value] -> Builder.Builder
encodeList encode values = cborArray (length values) <> foldMap encode values

getList :: Get.Get value -> Get.Get [value]
getList getValue = getArrayLength >>= (`replicateM` getValue)

expectArray :: Int -> Get.Get ()
expectArray expected = do
  actual <- getArrayLength
  unless (actual == expected) (fail "unexpected CBOR array length")
