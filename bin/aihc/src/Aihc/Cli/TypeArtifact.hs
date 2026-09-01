module Aihc.Cli.TypeArtifact
  ( TypeArtifact (..),
    decodeTypeArtifact,
    encodeTypeArtifact,
    encodeTypeArtifactFromInterface,
    encodeTypeInterface,
  )
where

import Aihc.Resolve (PackageId (..), ResolutionNamespace (..))
import Aihc.Tc
  ( ClassInfo (..),
    DataConFieldInfo (..),
    DataConFieldUnpack (..),
    DataConInfo (..),
    DataConSourceForm (..),
    DataFamilyInstanceInfo (..),
    DataTypeInfo (..),
    InstanceInfo (..),
    Pred (..),
    TcInterface (..),
    TcTermKey (..),
    TcType (..),
    TyCon,
    TyConFlavor (..),
    TyConInfo (..),
    TyVarId (..),
    TypeFamilyInstanceInfo (..),
    TypeScheme (..),
    Unique (..),
    tvKind,
    tyConArity,
    tyConName,
  )
import Aihc.Tc.Env (TypeSynonymInfo (..))
import Aihc.Tc.Types (mkTyConWithNamespace, setTyVarKind, tyConModuleName, tyConNamespace, tyConPackageId)
import Control.Monad (replicateM, unless)
import Data.Array (Array, listArray, (!))
import Data.Binary.Get qualified as Get
import Data.Bits (shiftR)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Builder.Extra (toLazyByteStringWith, untrimmedStrategy)
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Word (Word64, Word8)

data TypeArtifact = TypeArtifact
  { typeArtifactModuleName :: !Text,
    typeArtifactInputHashes :: ![(Text, Text)],
    typeArtifactInterface :: TcInterface
  }
  deriving (Show)

type TyConTable = Array Int TyCon

encodeTypeArtifact :: TypeArtifact -> BL.ByteString
encodeTypeArtifact artifact =
  encodeTypeArtifactFromInterface
    (typeArtifactModuleName artifact)
    (typeArtifactInputHashes artifact)
    (encodeTypeInterface (typeArtifactInterface artifact))

-- | Wrap encoded interface bytes as a type artifact.
-- The interface bytes are CBOR items 4 and 5 of the artifact array.
encodeTypeArtifactFromInterface :: Text -> [(Text, Text)] -> BL.ByteString -> BL.ByteString
encodeTypeArtifactFromInterface name hashes interfaceBytes =
  builderBytes
    ( cborArray 5
        <> cborText "aihc-type"
        <> cborText name
        <> encodeList encodeHash hashes
    )
    <> interfaceBytes
  where
    encodeHash (hashName, digest) = cborArray 2 <> cborText hashName <> cborText digest

encodeTypeInterface :: TcInterface -> BL.ByteString
encodeTypeInterface interface =
  let tyCons = Set.toAscList (interfaceTyCons interface)
      tyConTable = Map.fromList (zip tyCons [0 ..])
   in builderBytes (encodeList putTyConDefinition tyCons <> putInterface tyConTable interface)

builderBytes :: Builder.Builder -> BL.ByteString
builderBytes = toLazyByteStringWith (untrimmedStrategy 32768 32768) mempty
{-# INLINE builderBytes #-}

decodeTypeArtifact :: BL.ByteString -> TypeArtifact
decodeTypeArtifact = Get.runGet getArtifact

getArtifact :: Get.Get TypeArtifact
getArtifact = do
  expectArray 5
  expectText "aihc-type"
  typeArtifactModuleName <- getText
  typeArtifactInputHashes <- getList getHash
  tyCons <- getList getTyConDefinition
  let tyConTable = listArray (0, length tyCons - 1) tyCons
  interfaceBytes <- Get.getRemainingLazyByteString
  let typeArtifactInterface = Get.runGet (getInterface tyConTable) interfaceBytes
  pure TypeArtifact {typeArtifactModuleName, typeArtifactInputHashes, typeArtifactInterface}
  where
    getHash = expectArray 2 >> ((,) <$> getText <*> getText)

putInterface :: Map TyCon Word64 -> TcInterface -> Builder.Builder
putInterface table interface =
  cborArray 7
    <> encodeList (putTerm table) (tcInterfaceTerms interface)
    <> encodeList (putTyConInfo table) (tcInterfaceTyCons interface)
    <> encodeList (putDataTypeInfo table) (tcInterfaceDataTypes interface)
    <> encodeList (putClassInfo table) (tcInterfaceClasses interface)
    <> encodeList (putInstanceInfo table) (tcInterfaceInstances interface)
    <> encodeList (putDataFamilyInstanceInfo table) (tcInterfaceDataFamilyInstances interface)
    <> encodeList (putTypeFamilyInstanceInfo table) (tcInterfaceTypeFamilyInstances interface)

getInterface :: TyConTable -> Get.Get TcInterface
getInterface table = do
  length' <- getArrayLength
  tcInterfaceTerms <- getList (getTerm table)
  tcInterfaceTyCons <- getList (getTyConInfo table)
  tcInterfaceDataTypes <- getList (getDataTypeInfo table)
  tcInterfaceClasses <- getList (getClassInfo table)
  tcInterfaceInstances <- getList (getInstanceInfo table)
  tcInterfaceDataFamilyInstances <- getList (getDataFamilyInstanceInfo table)
  tcInterfaceTypeFamilyInstances <-
    case length' of
      6 -> pure []
      7 -> getList (getTypeFamilyInstanceInfo table)
      _ -> fail ("unsupported type interface array length: " <> show length')
  pure TcInterface {tcInterfaceTerms, tcInterfaceTyCons, tcInterfaceDataTypes, tcInterfaceClasses, tcInterfaceInstances, tcInterfaceDataFamilyInstances, tcInterfaceTypeFamilyInstances}

putTerm :: Map TyCon Word64 -> (TcTermKey, TypeScheme) -> Builder.Builder
putTerm table (key, scheme) = cborArray 2 <> putTermKey key <> putTypeScheme table scheme

getTerm :: TyConTable -> Get.Get (TcTermKey, TypeScheme)
getTerm table = expectArray 2 >> ((,) <$> getTermKey <*> getTypeScheme table)

putTermKey :: TcTermKey -> Builder.Builder
putTermKey key = case key of
  TcTermLocal unique -> cborArray 2 <> cborWord 0 <> cborInt unique
  TcTermGlobal (PackageId packageId) moduleName identifier -> cborArray 4 <> cborWord 1 <> cborText packageId <> cborText moduleName <> cborText identifier

getTermKey :: Get.Get TcTermKey
getTermKey = do
  length' <- getArrayLength
  tag <- getWord
  case (length', tag) of
    (2, 0) -> TcTermLocal <$> getInt
    (4, 1) -> (TcTermGlobal . PackageId <$> getText) <*> getText <*> getText
    _ -> fail "unsupported term key"

putTypeScheme :: Map TyCon Word64 -> TypeScheme -> Builder.Builder
putTypeScheme table (ForAll variables predicates body) = cborArray 3 <> encodeList (putTyVar table) variables <> encodeList (putPred table) predicates <> putType table body

getTypeScheme :: TyConTable -> Get.Get TypeScheme
getTypeScheme table = expectArray 3 >> (ForAll <$> getList (getTyVar table) <*> getList (getPred table) <*> getType table)

putTyVar :: Map TyCon Word64 -> TyVarId -> Builder.Builder
putTyVar table variable = cborArray 3 <> cborText (tvName variable) <> putUnique (tvUnique variable) <> putType table (tvKind variable)

getTyVar :: TyConTable -> Get.Get TyVarId
getTyVar table = do
  expectArray 3
  name <- getText
  unique <- getUnique
  kind <- getType table
  pure (setTyVarKind kind (TyVarId name unique))

putUnique :: Unique -> Builder.Builder
putUnique (Unique value) = cborInt value

getUnique :: Get.Get Unique
getUnique = Unique <$> getInt

putTyConDefinition :: TyCon -> Builder.Builder
putTyConDefinition tyCon =
  cborArray 5
    <> putPackageId (tyConPackageId tyCon)
    <> cborText (tyConModuleName tyCon)
    <> putResolutionNamespace (tyConNamespace tyCon)
    <> cborText (tyConName tyCon)
    <> cborInt (tyConArity tyCon)

getTyConDefinition :: Get.Get TyCon
getTyConDefinition = do
  expectArray 5
  packageId <- getPackageId
  moduleName <- getText
  namespace <- getResolutionNamespace
  mkTyConWithNamespace namespace packageId moduleName <$> getText <*> getInt

putTyCon :: Map TyCon Word64 -> TyCon -> Builder.Builder
putTyCon table tyCon = cborWord (Map.findWithDefault (error "missing type constructor index") tyCon table)
{-# INLINE putTyCon #-}

getTyCon :: TyConTable -> Get.Get TyCon
getTyCon table = (table !) . fromIntegral <$> getWord

putResolutionNamespace :: ResolutionNamespace -> Builder.Builder
putResolutionNamespace namespace =
  cborWord $
    case namespace of
      ResolutionNamespaceTerm -> 0
      ResolutionNamespaceType -> 1
      ResolutionNamespaceModule -> 2

getResolutionNamespace :: Get.Get ResolutionNamespace
getResolutionNamespace = do
  tag <- getWord
  case tag of
    0 -> pure ResolutionNamespaceTerm
    1 -> pure ResolutionNamespaceType
    2 -> pure ResolutionNamespaceModule
    _ -> fail "unsupported resolution namespace"

putPackageId :: PackageId -> Builder.Builder
putPackageId (PackageId identity) = cborText identity

getPackageId :: Get.Get PackageId
getPackageId = PackageId <$> getText

putType :: Map TyCon Word64 -> TcType -> Builder.Builder
putType table ty = case ty of
  TcTyVar variable -> sum1 0 (putTyVar table variable)
  TcMetaTv unique -> sum1 1 (putUnique unique)
  TcTyCon tyCon arguments -> sum2 2 (putTyCon table tyCon) (encodeList (putType table) arguments)
  TcFunTy argument result -> sum2 3 (putType table argument) (putType table result)
  TcForAllTy variable body -> sum2 4 (putTyVar table variable) (putType table body)
  TcQualTy predicates body -> sum2 5 (encodeList (putPred table) predicates) (putType table body)
  TcAppTy function argument -> sum2 6 (putType table function) (putType table argument)
{-# INLINE putType #-}

getType :: TyConTable -> Get.Get TcType
getType table = do
  length' <- getArrayLength
  tag <- getWord
  case (length', tag) of
    (2, 0) -> TcTyVar <$> getTyVar table
    (2, 1) -> TcMetaTv <$> getUnique
    (3, 2) -> TcTyCon <$> getTyCon table <*> getList (getType table)
    (3, 3) -> TcFunTy <$> getType table <*> getType table
    (3, 4) -> TcForAllTy <$> getTyVar table <*> getType table
    (3, 5) -> TcQualTy <$> getList (getPred table) <*> getType table
    (3, 6) -> TcAppTy <$> getType table <*> getType table
    _ -> fail "unsupported type"

putPred :: Map TyCon Word64 -> Pred -> Builder.Builder
putPred table predicate = case predicate of
  ClassPred tyCon arguments -> sum2 0 (putTyCon table tyCon) (encodeList (putType table) arguments)
  EqPred left right -> sum2 1 (putType table left) (putType table right)
  QuantifiedPred variables antecedents consequent ->
    cborArray 4 <> cborWord 2 <> encodeList (putTyVar table) variables <> encodeList (putPred table) antecedents <> putPred table consequent

getPred :: TyConTable -> Get.Get Pred
getPred table = do
  length' <- getArrayLength
  tag <- getWord
  case (length', tag) of
    (3, 0) -> ClassPred <$> getTyCon table <*> getList (getType table)
    (3, 1) -> EqPred <$> getType table <*> getType table
    (4, 2) -> QuantifiedPred <$> getList (getTyVar table) <*> getList (getPred table) <*> getPred table
    _ -> fail "unsupported predicate"

putTyConInfo :: Map TyCon Word64 -> TyConInfo -> Builder.Builder
putTyConInfo table info = cborArray 6 <> cborText (tciName info) <> cborInt (tciArity info) <> putTyCon table (tciTyCon info) <> putTypeScheme table (tciKindScheme info) <> putTyConFlavor (tciFlavor info) <> putMaybe (putTypeSynonymInfo table) (tciTypeSynonym info)

getTyConInfo :: TyConTable -> Get.Get TyConInfo
getTyConInfo table = do
  expectArray 6
  tciName <- getText
  tciArity <- getInt
  tciTyCon <- getTyCon table
  tciKindScheme <- getTypeScheme table
  tciFlavor <- getTyConFlavor
  tciTypeSynonym <- getMaybe (getTypeSynonymInfo table)
  pure TyConInfo {tciName, tciArity, tciTyCon, tciKindScheme, tciFlavor, tciTypeSynonym}

putTypeSynonymInfo :: Map TyCon Word64 -> TypeSynonymInfo -> Builder.Builder
putTypeSynonymInfo table info = cborArray 2 <> encodeList (putTyVar table) (tsiParams info) <> putMaybe (putType table) (tsiBody info)

getTypeSynonymInfo :: TyConTable -> Get.Get TypeSynonymInfo
getTypeSynonymInfo table = expectArray 2 >> (TypeSynonymInfo <$> getList (getTyVar table) <*> getMaybe (getType table))

putDataTypeInfo :: Map TyCon Word64 -> DataTypeInfo -> Builder.Builder
putDataTypeInfo table info = cborArray 6 <> cborText (dtiName info) <> putTyCon table (dtiTyCon info) <> encodeList (putTyVar table) (dtiTyVars info) <> putType table (dtiResultKind info) <> putTyConFlavor (dtiFlavor info) <> encodeList (putDataConInfo table) (dtiConstructors info)

getDataTypeInfo :: TyConTable -> Get.Get DataTypeInfo
getDataTypeInfo table = do
  expectArray 6
  dtiName <- getText
  dtiTyCon <- getTyCon table
  dtiTyVars <- getList (getTyVar table)
  dtiResultKind <- getType table
  dtiFlavor <- getTyConFlavor
  dtiConstructors <- getList (getDataConInfo table)
  pure DataTypeInfo {dtiName, dtiTyCon, dtiTyVars, dtiResultKind, dtiFlavor, dtiConstructors}

putDataConInfo :: Map TyCon Word64 -> DataConInfo -> Builder.Builder
putDataConInfo table info =
  cborArray 8
    <> cborText (dciName info)
    <> putOrigin (dciOrigin info)
    <> encodeList (putTyVar table) (dciUnivTyVars info)
    <> encodeList (putTyVar table) (dciExTyVars info)
    <> encodeList (putPred table) (dciTheta info)
    <> encodeList (putDataConFieldInfo table) (dciFields info)
    <> putType table (dciResTy info)
    <> putDataConSourceForm (dciSourceForm info)

getDataConInfo :: TyConTable -> Get.Get DataConInfo
getDataConInfo table = do
  expectArray 8
  dciName <- getText
  dciOrigin <- getOrigin
  dciUnivTyVars <- getList (getTyVar table)
  dciExTyVars <- getList (getTyVar table)
  dciTheta <- getList (getPred table)
  dciFields <- getList (getDataConFieldInfo table)
  dciResTy <- getType table
  dciSourceForm <- getDataConSourceForm
  pure DataConInfo {dciName, dciOrigin, dciUnivTyVars, dciExTyVars, dciTheta, dciFields, dciResTy, dciSourceForm}

putDataConFieldInfo :: Map TyCon Word64 -> DataConFieldInfo -> Builder.Builder
putDataConFieldInfo table info = cborArray 5 <> putMaybe cborText (dcfiLabel info) <> putType table (dcfiType info) <> putBool (dcfiStrict info) <> putBool (dcfiLazy info) <> putDataConFieldUnpack (dcfiUnpack info)

getDataConFieldInfo :: TyConTable -> Get.Get DataConFieldInfo
getDataConFieldInfo table = do
  expectArray 5
  dcfiLabel <- getMaybe getText
  dcfiType <- getType table
  dcfiStrict <- getBool
  dcfiLazy <- getBool
  dcfiUnpack <- getDataConFieldUnpack
  pure DataConFieldInfo {dcfiLabel, dcfiType, dcfiStrict, dcfiLazy, dcfiUnpack}

putClassInfo :: Map TyCon Word64 -> ClassInfo -> Builder.Builder
putClassInfo table info =
  cborArray 9
    <> cborText (ciName info)
    <> putTyCon table (ciTyCon info)
    <> putMaybe putTextOrigin (ciOrigin info)
    <> encodeList (putTyVar table) (ciKindTyVars info)
    <> encodeList (putTyVar table) (ciTyVars info)
    <> encodeList (putType table) (ciSuperClassTypes info)
    <> encodeList (putNamedScheme table) (ciMethods info)
    <> encodeList cborText (ciDefaultMethods info)
    <> encodeList (putNamedScheme table) (ciDefaultSignatures info)

getClassInfo :: TyConTable -> Get.Get ClassInfo
getClassInfo table = do
  expectArray 9
  ciName <- getText
  ciTyCon <- getTyCon table
  ciOrigin <- getMaybe getTextOrigin
  ciKindTyVars <- getList (getTyVar table)
  ciTyVars <- getList (getTyVar table)
  ciSuperClassTypes <- getList (getType table)
  ciMethods <- getList (getNamedScheme table)
  ciDefaultMethods <- getList getText
  ciDefaultSignatures <- getList (getNamedScheme table)
  pure ClassInfo {ciName, ciTyCon, ciOrigin, ciKindTyVars, ciTyVars, ciSuperClassTypes, ciMethods, ciDefaultMethods, ciDefaultSignatures}

putInstanceInfo :: Map TyCon Word64 -> InstanceInfo -> Builder.Builder
putInstanceInfo table info =
  cborArray 7
    <> cborText (iiClassName info)
    <> cborText (iiDictName info)
    <> putTextOrigin (iiDictOrigin info)
    <> putType table (iiDictType info)
    <> encodeList (putTyVar table) (iiTyVars info)
    <> encodeList (putPred table) (iiContext info)
    <> encodeList (putType table) (iiHead info)

getInstanceInfo :: TyConTable -> Get.Get InstanceInfo
getInstanceInfo table = do
  expectArray 7
  iiClassName <- getText
  iiDictName <- getText
  iiDictOrigin <- getTextOrigin
  iiDictType <- getType table
  iiTyVars <- getList (getTyVar table)
  iiContext <- getList (getPred table)
  iiHead <- getList (getType table)
  pure InstanceInfo {iiClassName, iiDictName, iiDictOrigin, iiDictType, iiTyVars, iiContext, iiHead}

putDataFamilyInstanceInfo :: Map TyCon Word64 -> DataFamilyInstanceInfo -> Builder.Builder
putDataFamilyInstanceInfo table info =
  cborArray 7
    <> cborText (dfiiFamilyName info)
    <> putType table (dfiiFamilyType info)
    <> encodeList (putTyVar table) (dfiiTyVars info)
    <> putTyCon table (dfiiRepresentationTyCon info)
    <> cborText (dfiiAxiomName info)
    <> encodeList cborText (dfiiConstructorNames info)
    <> putBool (dfiiIsNewtype info)

getDataFamilyInstanceInfo :: TyConTable -> Get.Get DataFamilyInstanceInfo
getDataFamilyInstanceInfo table = do
  expectArray 7
  dfiiFamilyName <- getText
  dfiiFamilyType <- getType table
  dfiiTyVars <- getList (getTyVar table)
  dfiiRepresentationTyCon <- getTyCon table
  dfiiAxiomName <- getText
  dfiiConstructorNames <- getList getText
  dfiiIsNewtype <- getBool
  pure DataFamilyInstanceInfo {dfiiFamilyName, dfiiFamilyType, dfiiTyVars, dfiiRepresentationTyCon, dfiiAxiomName, dfiiConstructorNames, dfiiIsNewtype}

putTypeFamilyInstanceInfo :: Map TyCon Word64 -> TypeFamilyInstanceInfo -> Builder.Builder
putTypeFamilyInstanceInfo table info =
  cborArray 7
    <> cborText (tfiiFamilyName info)
    <> cborText (tfiiAxiomName info)
    <> putOrigin (tfiiOrigin info)
    <> encodeList (putTyVar table) (tfiiTyVars info)
    <> putType table (tfiiLeft info)
    <> putType table (tfiiRight info)
    <> putBool (tfiiClosed info)

getTypeFamilyInstanceInfo :: TyConTable -> Get.Get TypeFamilyInstanceInfo
getTypeFamilyInstanceInfo table = do
  expectArray 7
  tfiiFamilyName <- getText
  tfiiAxiomName <- getText
  tfiiOrigin <- getOrigin
  tfiiTyVars <- getList (getTyVar table)
  tfiiLeft <- getType table
  tfiiRight <- getType table
  tfiiClosed <- getBool
  pure TypeFamilyInstanceInfo {tfiiFamilyName, tfiiAxiomName, tfiiOrigin, tfiiTyVars, tfiiLeft, tfiiRight, tfiiClosed}

putOrigin :: (PackageId, Text) -> Builder.Builder
putOrigin (packageId, moduleName) = cborArray 2 <> putPackageId packageId <> cborText moduleName

getOrigin :: Get.Get (PackageId, Text)
getOrigin = expectArray 2 >> ((,) <$> getPackageId <*> getText)

putTextOrigin :: (Text, Text) -> Builder.Builder
putTextOrigin (packageId, moduleName) = cborArray 2 <> cborText packageId <> cborText moduleName

getTextOrigin :: Get.Get (Text, Text)
getTextOrigin = expectArray 2 >> ((,) <$> getText <*> getText)

putNamedScheme :: Map TyCon Word64 -> (Text, TypeScheme) -> Builder.Builder
putNamedScheme table (name, scheme) = cborArray 2 <> cborText name <> putTypeScheme table scheme

getNamedScheme :: TyConTable -> Get.Get (Text, TypeScheme)
getNamedScheme table = expectArray 2 >> ((,) <$> getText <*> getTypeScheme table)

interfaceTyCons :: TcInterface -> Set.Set TyCon
interfaceTyCons interface =
  addTypeFamilyInstanceInfos (tcInterfaceTypeFamilyInstances interface) $
    addDataFamilyInstanceInfos (tcInterfaceDataFamilyInstances interface) $
      addInstanceInfos (tcInterfaceInstances interface) $
        addClassInfos (tcInterfaceClasses interface) $
          addDataTypeInfos (tcInterfaceDataTypes interface) $
            addTyConInfos (tcInterfaceTyCons interface) $
              addTerms (tcInterfaceTerms interface) Set.empty

addTerms :: [(TcTermKey, TypeScheme)] -> Set.Set TyCon -> Set.Set TyCon
addTerms terms acc = foldl' (\tyCons (_, scheme) -> addTypeScheme scheme tyCons) acc terms

addTyConInfos :: [TyConInfo] -> Set.Set TyCon -> Set.Set TyCon
addTyConInfos = flip (foldl' (flip addTyConInfo))

addTyConInfo :: TyConInfo -> Set.Set TyCon -> Set.Set TyCon
addTyConInfo info acc =
  addMaybe addTypeSynonymInfo (tciTypeSynonym info) $
    addTypeScheme (tciKindScheme info) (Set.insert (tciTyCon info) acc)

addTypeSynonymInfo :: TypeSynonymInfo -> Set.Set TyCon -> Set.Set TyCon
addTypeSynonymInfo info acc =
  addMaybe addType (tsiBody info) (addTyVars (tsiParams info) acc)

addDataTypeInfos :: [DataTypeInfo] -> Set.Set TyCon -> Set.Set TyCon
addDataTypeInfos = flip (foldl' (flip addDataTypeInfo))

addDataTypeInfo :: DataTypeInfo -> Set.Set TyCon -> Set.Set TyCon
addDataTypeInfo info acc =
  addDataConInfos (dtiConstructors info) $
    addType (dtiResultKind info) $
      addTyVars (dtiTyVars info) (Set.insert (dtiTyCon info) acc)

addDataConInfos :: [DataConInfo] -> Set.Set TyCon -> Set.Set TyCon
addDataConInfos = flip (foldl' (flip addDataConInfo))

addDataConInfo :: DataConInfo -> Set.Set TyCon -> Set.Set TyCon
addDataConInfo info acc =
  addType (dciResTy info) $
    addDataConFields (dciFields info) $
      addPreds (dciTheta info) $
        addTyVars (dciUnivTyVars info <> dciExTyVars info) acc

addDataConFields :: [DataConFieldInfo] -> Set.Set TyCon -> Set.Set TyCon
addDataConFields = flip (foldl' (\tyCons field -> addType (dcfiType field) tyCons))

addClassInfos :: [ClassInfo] -> Set.Set TyCon -> Set.Set TyCon
addClassInfos = flip (foldl' (flip addClassInfo))

addClassInfo :: ClassInfo -> Set.Set TyCon -> Set.Set TyCon
addClassInfo info acc =
  addNamedSchemes (ciMethods info <> ciDefaultSignatures info) $
    addTypes (ciSuperClassTypes info) $
      addTyVars (ciKindTyVars info <> ciTyVars info) (Set.insert (ciTyCon info) acc)

addInstanceInfos :: [InstanceInfo] -> Set.Set TyCon -> Set.Set TyCon
addInstanceInfos = flip (foldl' (flip addInstanceInfo))

addInstanceInfo :: InstanceInfo -> Set.Set TyCon -> Set.Set TyCon
addInstanceInfo info acc =
  addTypes (iiHead info) $
    addPreds (iiContext info) $
      addTyVars (iiTyVars info) (addType (iiDictType info) acc)

addDataFamilyInstanceInfos :: [DataFamilyInstanceInfo] -> Set.Set TyCon -> Set.Set TyCon
addDataFamilyInstanceInfos = flip (foldl' (flip addDataFamilyInstanceInfo))

addDataFamilyInstanceInfo :: DataFamilyInstanceInfo -> Set.Set TyCon -> Set.Set TyCon
addDataFamilyInstanceInfo info acc =
  addTyVars (dfiiTyVars info) $
    addType (dfiiFamilyType info) (Set.insert (dfiiRepresentationTyCon info) acc)

addTypeFamilyInstanceInfos :: [TypeFamilyInstanceInfo] -> Set.Set TyCon -> Set.Set TyCon
addTypeFamilyInstanceInfos = flip (foldl' (flip addTypeFamilyInstanceInfo))

addTypeFamilyInstanceInfo :: TypeFamilyInstanceInfo -> Set.Set TyCon -> Set.Set TyCon
addTypeFamilyInstanceInfo info acc =
  addType (tfiiRight info) $
    addType (tfiiLeft info) (addTyVars (tfiiTyVars info) acc)

addNamedSchemes :: [(Text, TypeScheme)] -> Set.Set TyCon -> Set.Set TyCon
addNamedSchemes schemes acc = foldl' (\tyCons (_, scheme) -> addTypeScheme scheme tyCons) acc schemes

addTypeScheme :: TypeScheme -> Set.Set TyCon -> Set.Set TyCon
addTypeScheme (ForAll variables predicates body) acc =
  addType body (addPreds predicates (addTyVars variables acc))

addTyVars :: [TyVarId] -> Set.Set TyCon -> Set.Set TyCon
addTyVars = flip (foldl' (flip addTyVar))

addTyVar :: TyVarId -> Set.Set TyCon -> Set.Set TyCon
addTyVar variable = addType (tvKind variable)

addPreds :: [Pred] -> Set.Set TyCon -> Set.Set TyCon
addPreds = flip (foldl' (flip addPred))

addPred :: Pred -> Set.Set TyCon -> Set.Set TyCon
addPred predicate acc = case predicate of
  ClassPred tyCon arguments -> addTypes arguments (Set.insert tyCon acc)
  EqPred left right -> addType right (addType left acc)
  QuantifiedPred variables antecedents consequent ->
    addPred consequent (addPreds antecedents (addTyVars variables acc))

addTypes :: [TcType] -> Set.Set TyCon -> Set.Set TyCon
addTypes = flip (foldl' (flip addType))

addType :: TcType -> Set.Set TyCon -> Set.Set TyCon
addType ty acc = case ty of
  TcTyVar variable -> addTyVar variable acc
  TcMetaTv {} -> acc
  TcTyCon tyCon arguments -> addTypes arguments (Set.insert tyCon acc)
  TcFunTy argument result -> addType result (addType argument acc)
  TcForAllTy variable body -> addType body (addTyVar variable acc)
  TcQualTy predicates body -> addType body (addPreds predicates acc)
  TcAppTy function argument -> addType argument (addType function acc)
{-# INLINE addType #-}

addMaybe :: (value -> Set.Set TyCon -> Set.Set TyCon) -> Maybe value -> Set.Set TyCon -> Set.Set TyCon
addMaybe add value acc = case value of
  Nothing -> acc
  Just item -> add item acc

encodeList :: (value -> Builder.Builder) -> [value] -> Builder.Builder
encodeList encode values = cborArray (length values) <> foldMap encode values
{-# INLINE encodeList #-}

getList :: Get.Get value -> Get.Get [value]
getList getValue = getArrayLength >>= (`replicateM` getValue)

putMaybe :: (value -> Builder.Builder) -> Maybe value -> Builder.Builder
putMaybe encode value = case value of
  Nothing -> cborArray 1 <> cborWord 0
  Just item -> cborArray 2 <> cborWord 1 <> encode item

getMaybe :: Get.Get value -> Get.Get (Maybe value)
getMaybe getValue = do
  length' <- getArrayLength
  tag <- getWord
  case (length', tag) of
    (1, 0) -> pure Nothing
    (2, 1) -> Just <$> getValue
    _ -> fail "unsupported optional value"

putBool :: Bool -> Builder.Builder
putBool value = cborWord (if value then 1 else 0)

getBool :: Get.Get Bool
getBool = do
  value <- getWord
  case value of
    0 -> pure False
    1 -> pure True
    _ -> fail "unsupported Boolean value"

putTyConFlavor :: TyConFlavor -> Builder.Builder
putTyConFlavor flavor = cborWord $ case flavor of
  ClassTyCon -> 0
  DataTyCon -> 1
  DataFamilyTyCon -> 2
  NewtypeTyCon -> 3
  SynonymTyCon -> 4
  TypeFamilyTyCon -> 5

getTyConFlavor :: Get.Get TyConFlavor
getTyConFlavor = getTagged "type constructor flavor" [(0, ClassTyCon), (1, DataTyCon), (2, DataFamilyTyCon), (3, NewtypeTyCon), (4, SynonymTyCon), (5, TypeFamilyTyCon)]

putDataConSourceForm :: DataConSourceForm -> Builder.Builder
putDataConSourceForm sourceForm = cborWord $ case sourceForm of
  PrefixDataCon -> 0
  InfixDataCon -> 1
  RecordDataCon -> 2

getDataConSourceForm :: Get.Get DataConSourceForm
getDataConSourceForm = getTagged "constructor source form" [(0, PrefixDataCon), (1, InfixDataCon), (2, RecordDataCon)]

putDataConFieldUnpack :: DataConFieldUnpack -> Builder.Builder
putDataConFieldUnpack unpack = cborWord $ case unpack of
  NoFieldUnpack -> 0
  UnpackField -> 1
  NoUnpackField -> 2

getDataConFieldUnpack :: Get.Get DataConFieldUnpack
getDataConFieldUnpack = getTagged "field unpack mode" [(0, NoFieldUnpack), (1, UnpackField), (2, NoUnpackField)]

getTagged :: String -> [(Word64, value)] -> Get.Get value
getTagged label values = do
  tag <- getWord
  case lookup tag values of
    Just value -> pure value
    Nothing -> fail ("unsupported " <> label)

sum1 :: Word64 -> Builder.Builder -> Builder.Builder
sum1 tag first = cborArray 2 <> cborWord tag <> first
{-# INLINE sum1 #-}

sum2 :: Word64 -> Builder.Builder -> Builder.Builder -> Builder.Builder
sum2 tag first second = cborArray 3 <> cborWord tag <> first <> second
{-# INLINE sum2 #-}

expectArray :: Int -> Get.Get ()
expectArray expected = do
  actual <- getArrayLength
  unless (actual == expected) (fail "unexpected CBOR array length")

expectText :: Text -> Get.Get ()
expectText expected = do
  actual <- getText
  unless (actual == expected) (fail "unexpected artifact kind")

cborArray :: Int -> Builder.Builder
cborArray = cborMajor 4 . fromIntegral
{-# INLINE cborArray #-}

cborText :: Text -> Builder.Builder
cborText value = cborMajor 3 (fromIntegral (BS.length bytes)) <> Builder.byteString bytes
  where
    bytes = TE.encodeUtf8 value
{-# INLINE cborText #-}

cborInt :: Int -> Builder.Builder
cborInt value
  | value >= 0 = cborMajor 0 (fromIntegral value)
  | otherwise = cborMajor 1 (fromIntegral (-1 - value))
{-# INLINE cborInt #-}

cborWord :: Word64 -> Builder.Builder
cborWord = cborMajor 0
{-# INLINE cborWord #-}

cborMajor :: Word8 -> Word64 -> Builder.Builder
cborMajor major value
  | value < 24 = Builder.word8 (major * 32 + fromIntegral value)
  | value <= 255 = Builder.word8 (major * 32 + 24) <> Builder.word8 (fromIntegral value)
  | value <= 65535 = Builder.word8 (major * 32 + 25) <> Builder.word16BE (fromIntegral value)
  | value <= 4294967295 = Builder.word8 (major * 32 + 26) <> Builder.word32BE (fromIntegral value)
  | otherwise = Builder.word8 (major * 32 + 27) <> Builder.word64BE value
{-# INLINE cborMajor #-}

getArrayLength :: Get.Get Int
getArrayLength = fromIntegral <$> getMajor 4

getText :: Get.Get Text
getText = do
  length' <- getMajor 3
  TE.decodeUtf8 <$> Get.getByteString (fromIntegral length')

getInt :: Get.Get Int
getInt = do
  initial <- Get.lookAhead Get.getWord8
  let major = initial `shiftR` 5
  value <- getMajor major
  case major of
    0 -> pure (fromIntegral value)
    1 -> pure (-1 - fromIntegral value)
    _ -> fail "unexpected CBOR integer"

getWord :: Get.Get Word64
getWord = getMajor 0

getMajor :: Word8 -> Get.Get Word64
getMajor expected = do
  initial <- Get.getWord8
  let major = initial `shiftR` 5
      info = initial `mod` 32
  unless (major == expected) (fail "unexpected CBOR major type")
  case info of
    value | value < 24 -> pure (fromIntegral value)
    24 -> fromIntegral <$> Get.getWord8
    25 -> fromIntegral <$> Get.getWord16be
    26 -> fromIntegral <$> Get.getWord32be
    27 -> Get.getWord64be
    _ -> fail "unsupported CBOR length"
