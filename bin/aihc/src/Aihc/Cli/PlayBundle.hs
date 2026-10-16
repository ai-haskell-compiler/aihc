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
--   resolve artifact of every module, the table of every type
--   constructor, and the index of the type section: the key of every
--   fact with the offset and the length of its bytes.
--
-- * The type section, decoded one fact at a time. Each fact is the body
--   of a type interface with one entry, in the format of
--   "Aihc.Cli.TypeArtifact", against the type constructor table of the
--   header. The helper decodes only the facts the type checker looks up.
--
-- The loaded interface maps hold a thunk for each fact. The type checker
-- forces a thunk when it looks the fact up, and the decoder runs then.
-- Two kinds of fact get a record at once, with thunks in the fields the
-- type checker does not read at the start: the type constructors, which
-- the type checker scans by name, and the instances, whose classes the
-- instance environment reads when it is built.
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

import Aihc.Cbor (cborArray, cborBytes, cborInt, cborText, getArrayLength, getInt, getMajor, getText, (<*!>))
import Aihc.Cli.InterfaceTyCons (interfaceTyCons)
import Aihc.Cli.ResolveArtifact (ResolveArtifact (..), decodeResolveArtifact)
import Aihc.Cli.TypeArtifact (decodeTypeInterfaceBody, encodeTypeInterfaceBody, getTermKey, getTyConDefinition, getTyConFlavor, putTermKey, putTyConDefinition, putTyConFlavor)
import Aihc.Resolve (Entity (..), ModuleExports, ModuleKey (..), Package (..), PackageId (..), moduleExportsFromList)
import Aihc.Tc
  ( ClassInfo (..),
    DataTypeInfo (..),
    InstanceInfo (..),
    MergeCheck (..),
    TcAxiomKey (..),
    TcInterface (..),
    TyCon,
    TyConFlavor,
    TyConInfo (..),
    emptyTcInterface,
    mergeTcInterfaces,
    tyConArity,
    tyConKey,
  )
import Control.Monad (replicateM, unless, (<$!>))
import Data.Array (Array, listArray, (!))
import Data.Binary.Get qualified as Get
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as BL
import Data.Map.Lazy qualified as LazyMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)

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
    -- fact, or a record with such thunks in its fields.
    loadedInterface :: !TcInterface,
    loadedStats :: !BundleStats
  }

data BundleStats = BundleStats
  { statsModules :: !Int,
    statsNamingBytes :: !Int,
    statsTyCons :: !Int,
    statsTypeBytes :: !Int,
    statsFacts :: !Int
  }
  deriving (Show)

bundleMagic :: Text
bundleMagic = "aihc-play-bundle-2"

-- | Where the bytes of one fact are in the type section: the offset and
-- the length.
data Place = Place !Int !Int

-- | The index of the type section, one list per kind of fact. A type
-- constructor is a number in the table of the header.
data Index = Index
  { indexTerms :: [(Entity, Place)],
    indexTyCons :: [(Int, Text, TyConFlavor, Place)],
    indexDataTypes :: [(Int, Place)],
    indexClasses :: [(Int, Place)],
    indexInstances :: [(Int, (Text, Text), Text, Place)],
    indexDataFamilyInstances :: [(TcAxiomKey, Place)],
    indexTypeFamilyInstances :: [(TcAxiomKey, Place)],
    indexPatSyns :: [(Entity, Place)],
    indexForeignImports :: [(Entity, Place)]
  }

-- | Write a bundle: the primitive package identity, the modules, and the
-- type interfaces of the modules.
encodeBundle :: PackageId -> [BundleModule] -> [TcInterface] -> BL.ByteString
encodeBundle prim modules interfaces =
  Builder.toLazyByteString (putHeader <> Builder.lazyByteString typeSection)
  where
    interface = mergeTcInterfaces TrustMergedFacts interfaces
    tyCons = Set.toAscList (interfaceTyCons interface)
    tyConTable = Map.fromList (zip tyCons [0 ..])
    (index, typeSection) = encodeTypeSection tyConTable interface
    putHeader =
      cborArray 7
        <> cborText bundleMagic
        <> putPackageId prim
        <> cborInt (tcInterfaceUniqueBound interface)
        <> encodeList putModule modules
        <> encodeList putTyConDefinition tyCons
        <> putIndex index
        <> cborInt (fromIntegral (BL.length typeSection))
    putModule modu =
      cborArray 4
        <> cborText (packageName (bundleModulePackage modu))
        <> putPackageId (packageId (bundleModulePackage modu))
        <> cborText (bundleModuleName modu)
        <> cborBytes (bundleModuleResolve modu)

-- | The type section and its index. Each fact becomes an interface with
-- that fact alone, written against the shared type constructor table,
-- so a fact carries the parts it needs and nothing else. The facts of
-- each kind are in key order, so the loader builds each map in one pass.
encodeTypeSection :: Map TyCon Word64 -> TcInterface -> (Index, BL.ByteString)
encodeTypeSection tyConTable interface =
  let (terms, afterTerms) = facts 0 (\k v -> emptyTcInterface {tcInterfaceTermMap = Map.singleton k v}) (tcInterfaceTermMap interface)
      (tyCons, afterTyCons) = facts afterTerms (\k v -> emptyTcInterface {tcInterfaceTyConMap = Map.singleton k v}) (tcInterfaceTyConMap interface)
      (dataTypes, afterDataTypes) = facts afterTyCons (\k v -> emptyTcInterface {tcInterfaceDataTypeMap = Map.singleton k v}) (tcInterfaceDataTypeMap interface)
      (classes, afterClasses) = facts afterDataTypes (\k v -> emptyTcInterface {tcInterfaceClassMap = Map.singleton k v}) (tcInterfaceClassMap interface)
      (instances, afterInstances) = facts afterClasses (\k v -> emptyTcInterface {tcInterfaceInstanceMap = Map.singleton k v}) (tcInterfaceInstanceMap interface)
      (dataFamilies, afterDataFamilies) = facts afterInstances (\k v -> emptyTcInterface {tcInterfaceDataFamilyInstanceMap = Map.singleton k v}) (tcInterfaceDataFamilyInstanceMap interface)
      (typeFamilies, afterTypeFamilies) = facts afterDataFamilies (\k v -> emptyTcInterface {tcInterfaceTypeFamilyInstanceMap = Map.singleton k v}) (tcInterfaceTypeFamilyInstanceMap interface)
      (patSyns, afterPatSyns) = facts afterTypeFamilies (\k v -> emptyTcInterface {tcInterfacePatSynMap = Map.singleton k v}) (tcInterfacePatSynMap interface)
      (foreignImports, afterForeignImports) = facts afterPatSyns (\k v -> emptyTcInterface {tcInterfaceForeignImportMap = Map.singleton k v}) (tcInterfaceForeignImportMap interface)
      index =
        Index
          { indexTerms = [(key, place) | (key, _, place) <- fst terms],
            indexTyCons = [(tyConNumber (tciTyCon info), tciName info, tciFlavor info, place) | (_, info, place) <- fst tyCons],
            indexDataTypes = [(tyConNumber (dtiTyCon info), place) | (_, info, place) <- fst dataTypes],
            indexClasses = [(tyConNumber (ciTyCon info), place) | (_, info, place) <- fst classes],
            indexInstances = [(tyConNumber (iiClass info), iiDictOrigin info, iiDictName info, place) | (_, info, place) <- fst instances],
            indexDataFamilyInstances = [(key, place) | (key, _, place) <- fst dataFamilies],
            indexTypeFamilyInstances = [(key, place) | (key, _, place) <- fst typeFamilies],
            indexPatSyns = [(key, place) | (key, _, place) <- fst patSyns],
            indexForeignImports = [(key, place) | (key, _, place) <- fst foreignImports]
          }
      section = snd terms <> snd tyCons <> snd dataTypes <> snd classes <> snd instances <> snd dataFamilies <> snd typeFamilies <> snd patSyns <> snd foreignImports
   in afterForeignImports `seq` (index, Builder.toLazyByteString section)
  where
    tyConNumber tyCon =
      maybe (error "the type constructor table misses a type constructor of a fact") fromIntegral (Map.lookup tyCon tyConTable)
    -- The entries of one map, each as its own interface, placed one
    -- after the other from the given offset.
    facts :: Int -> (key -> value -> TcInterface) -> Map key value -> (([(key, value, Place)], Builder.Builder), Int)
    facts start singleton values =
      let step (entries, builder, offset) (key, value) =
            let bytes = Builder.toLazyByteString (encodeTypeInterfaceBody tyConTable (singleton key value))
                size = fromIntegral (BL.length bytes)
             in ((key, value, Place offset size) : entries, builder <> Builder.lazyByteString bytes, offset + size)
          (entries', builder', end) = foldl step ([], mempty, start) (Map.toAscList values)
       in ((reverse entries', builder'), end)

putIndex :: Index -> Builder.Builder
putIndex index =
  cborArray 9
    <> encodeList (\(key, place) -> cborArray 3 <> putTermKey key <> putPlace place) (indexTerms index)
    <> encodeList (\(number, name, flavor, place) -> cborArray 5 <> cborInt number <> cborText name <> putTyConFlavor flavor <> putPlace place) (indexTyCons index)
    <> encodeList (\(number, place) -> cborArray 3 <> cborInt number <> putPlace place) (indexDataTypes index)
    <> encodeList (\(number, place) -> cborArray 3 <> cborInt number <> putPlace place) (indexClasses index)
    <> encodeList (\(number, (package, moduleName), dictName, place) -> cborArray 6 <> cborInt number <> cborText package <> cborText moduleName <> cborText dictName <> putPlace place) (indexInstances index)
    <> encodeList (\(key, place) -> cborArray 5 <> putAxiomKey key <> putPlace place) (indexDataFamilyInstances index)
    <> encodeList (\(key, place) -> cborArray 5 <> putAxiomKey key <> putPlace place) (indexTypeFamilyInstances index)
    <> encodeList (\(key, place) -> cborArray 3 <> putTermKey key <> putPlace place) (indexPatSyns index)
    <> encodeList (\(key, place) -> cborArray 3 <> putTermKey key <> putPlace place) (indexForeignImports index)
  where
    putPlace (Place offset size) = cborInt offset <> cborInt size
    putAxiomKey (TcAxiomKey package moduleName name) = putPackageId package <> cborText moduleName <> cborText name

getIndex :: Get.Get Index
getIndex = do
  expectArray 9
  Index
    <$!> getList (expectArray 3 >> ((,) <$!> getTermKey <*!> getPlace))
      <*!> getList (expectArray 5 >> ((,,,) <$!> getInt <*!> getText <*!> getTyConFlavor <*!> getPlace))
      <*!> getList (expectArray 3 >> ((,) <$!> getInt <*!> getPlace))
      <*!> getList (expectArray 3 >> ((,) <$!> getInt <*!> getPlace))
      <*!> getList (expectArray 6 >> ((,,,) <$!> getInt <*!> ((,) <$!> getText <*!> getText) <*!> getText <*!> getPlace))
      <*!> getList (expectArray 5 >> ((,) <$!> getAxiomKey <*!> getPlace))
      <*!> getList (expectArray 5 >> ((,) <$!> getAxiomKey <*!> getPlace))
      <*!> getList (expectArray 3 >> ((,) <$!> getTermKey <*!> getPlace))
      <*!> getList (expectArray 3 >> ((,) <$!> getTermKey <*!> getPlace))
  where
    getPlace = Place <$!> getInt <*!> getInt
    getAxiomKey = TcAxiomKey <$!> getPackageId <*!> getText <*!> getText

-- | Decode a bundle. The header is decoded now; the type section is kept
-- as bytes, and each fact is decoded when the type checker looks it up.
loadBundle :: BS.ByteString -> Either String LoadedBundle
loadBundle bytes =
  case Get.runGetOrFail getHeader (BL.fromStrict bytes) of
    Left (_, _, message) -> Left ("invalid bundle: " <> message)
    Right (_, consumed, header) -> do
      let section = BS.drop (fromIntegral consumed) bytes
      unless (BS.length section == headerTypeBytes header) $
        Left "invalid bundle: the type section has the wrong length"
      exports <- traverse moduleExports (headerModules header)
      let tyCons = headerTyCons header
          index = headerIndex header
          fact :: (TcInterface -> Map key value) -> Place -> value
          fact select (Place offset size) =
            case decodeTypeInterfaceBody tyCons (BL.fromStrict (BS.take size (BS.drop offset section))) of
              Left message -> error ("corrupt bundle fact at offset " <> show offset <> ": " <> message)
              Right decoded ->
                case Map.elems (select decoded) of
                  [value] -> value
                  _ -> error ("corrupt bundle fact at offset " <> show offset <> ": not one fact")
          tyConInfo (number, name, flavor, place) =
            let tyCon = tyCons ! number
                decoded = fact tcInterfaceTyConMap place
             in ( tyConKey tyCon,
                  TyConInfo
                    { tciName = name,
                      tciArity = tyConArity tyCon,
                      tciTyCon = tyCon,
                      tciKindScheme = tciKindScheme decoded,
                      tciFlavor = flavor,
                      tciTypeSynonym = tciTypeSynonym decoded,
                      tciInjectivity = tciInjectivity decoded
                    }
                )
          instanceInfo (number, origin, dictName, place) =
            let decoded = fact tcInterfaceInstanceMap place
             in ( (origin, dictName),
                  InstanceInfo
                    { iiClass = tyCons ! number,
                      iiDictName = dictName,
                      iiDictOrigin = origin,
                      iiDictType = iiDictType decoded,
                      iiTyVars = iiTyVars decoded,
                      iiContext = iiContext decoded,
                      iiHead = iiHead decoded
                    }
                )
          -- The global name of a type constructor is its key in the
          -- interface maps, so a type constructor number names a data
          -- type or a class too.
          byTyCon select entries = LazyMap.fromDistinctAscList [(tyConKey (tyCons ! number), fact select place) | (number, place) <- entries]
          byKey select entries = LazyMap.fromDistinctAscList [(key, fact select place) | (key, place) <- entries]
          interface =
            TcInterface
              { tcInterfaceTermMap = byKey tcInterfaceTermMap (indexTerms index),
                tcInterfaceTyConMap = LazyMap.fromDistinctAscList (map tyConInfo (indexTyCons index)),
                tcInterfaceDataTypeMap = byTyCon tcInterfaceDataTypeMap (indexDataTypes index),
                tcInterfaceClassMap = byTyCon tcInterfaceClassMap (indexClasses index),
                tcInterfaceInstanceMap = LazyMap.fromDistinctAscList (map instanceInfo (indexInstances index)),
                tcInterfaceDataFamilyInstanceMap = byKey tcInterfaceDataFamilyInstanceMap (indexDataFamilyInstances index),
                tcInterfaceTypeFamilyInstanceMap = byKey tcInterfaceTypeFamilyInstanceMap (indexTypeFamilyInstances index),
                tcInterfacePatSynMap = byKey tcInterfacePatSynMap (indexPatSyns index),
                tcInterfaceForeignImportMap = byKey tcInterfaceForeignImportMap (indexForeignImports index),
                tcInterfaceUniqueBound = headerUniqueBound header
              }
          facts =
            length (indexTerms index)
              + length (indexTyCons index)
              + length (indexDataTypes index)
              + length (indexClasses index)
              + length (indexInstances index)
              + length (indexDataFamilyInstances index)
              + length (indexTypeFamilyInstances index)
              + length (indexPatSyns index)
              + length (indexForeignImports index)
      pure
        LoadedBundle
          { loadedPrim = headerPrim header,
            loadedExports = moduleExportsFromList exports,
            loadedInterface = interface,
            loadedStats =
              BundleStats
                { statsModules = length (headerModules header),
                  statsNamingBytes = sum [BS.length (bundleModuleResolve modu) | modu <- headerModules header],
                  statsTyCons = length tyCons,
                  statsTypeBytes = BS.length section,
                  statsFacts = facts
                }
          }
  where
    moduleExports modu = do
      artifact <- either (\message -> Left ("invalid resolve artifact of " <> T.unpack (bundleModuleName modu) <> ": " <> message)) Right (decodeResolveArtifact (bundleModuleResolve modu))
      pure (ModuleKey (bundleModulePackage modu) (bundleModuleName modu), resolveArtifactExports artifact)

data Header = Header
  { headerPrim :: !PackageId,
    headerUniqueBound :: !Int,
    headerModules :: ![BundleModule],
    headerTyCons :: !(Array Int TyCon),
    headerIndex :: !Index,
    headerTypeBytes :: !Int
  }

getHeader :: Get.Get Header
getHeader = do
  expectArray 7
  magic <- getText
  unless (magic == bundleMagic) (fail ("not a bundle: " <> T.unpack magic))
  headerPrim <- getPackageId
  headerUniqueBound <- getInt
  headerModules <- getList getModule
  tyCons <- getList getTyConDefinition
  let headerTyCons = listArray (0, length tyCons - 1) tyCons
  headerIndex <- getIndex
  headerTypeBytes <- getInt
  pure Header {headerPrim, headerUniqueBound, headerModules, headerTyCons, headerIndex, headerTypeBytes}
  where
    getModule = do
      expectArray 4
      name <- getText
      identity <- getPackageId
      moduleName <- getText
      -- Not copied: the bytes are a slice of the bundle, which stays
      -- loaded for the type section anyway.
      BundleModule (Package name identity) moduleName <$> getSlice

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
